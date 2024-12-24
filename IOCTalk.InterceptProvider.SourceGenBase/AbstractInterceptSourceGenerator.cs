using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Diagnostics;
using System.Linq;
using System.Text;
using System.Threading;

namespace IOCTalk.InterceptProvider.SourceGenBase
{
    public abstract class AbstractInterceptSourceGenerator : IIncrementalGenerator
    {

        public const string MethodBodyIntention = "\t\t";
        public const string MethodLineIntention = "\t\t\t";
        public const string MethodLineIntention2 = "\t\t\t\t";

        protected const string InterceptedServiceFieldName = "interceptedService";
        protected const string MonitorHostFieldName = "monitorHost";

#if DEBUG
        protected bool isVerboseLogging = false;
        protected bool attachDebugger = false;
#endif


        public abstract string InterceptProviderName { get; }

        public abstract string InterceptProviderShortName { get; }


        public void Initialize(IncrementalGeneratorInitializationContext context)
        {
#if DEBUG
            if (attachDebugger == true)
            {
                if (!Debugger.IsAttached)
                {
                    Debugger.Launch();
                }
            }

            LogToFileHelper.WriteLog("Initialize SourceGenerator");
#endif

            var targetAssemblyName = context.CompilationProvider.Select(static (cp, _) => cp.AssemblyName);


            var interceptProviderRegistrations = context.SyntaxProvider
                        .CreateSyntaxProvider(CollectInterceptProviderCalls, GetInterceptInterfaceSymbol)
                        .Where(type => type is not null)
                        .Collect();

            var combined = interceptProviderRegistrations.Combine(targetAssemblyName);

            context.RegisterSourceOutput(combined, ExecuteGenerateCode);
        }

        //private void ExecuteGenerateCode(SourceProductionContext context, ImmutableArray<ITypeSymbol> input)
        private void ExecuteGenerateCode(SourceProductionContext context, (ImmutableArray<ITypeSymbol> interceptProviderRegistrations, string? targetAssemblyName) input)
        {
            var interceptProviderRegistrations = input.interceptProviderRegistrations.Distinct(SymbolEqualityComparer.Default).Cast<ITypeSymbol>().ToArray();
            var targetAssemblyName = input.targetAssemblyName;

#if DEBUG
            LogToFileHelper.WriteLog($"Intercept registration count: {interceptProviderRegistrations.Length}; Target assembly: {targetAssemblyName}");
#endif            
            Dictionary<ITypeSymbol, string> interfaceImplementationMapping = new Dictionary<ITypeSymbol, string>(SymbolEqualityComparer.Default);
            foreach (var interfaceType in interceptProviderRegistrations)
            {
                string className;
                var sourceCode = BuildInterfaceInterceptImplementationSource(interfaceType, targetAssemblyName, out className);

                interfaceImplementationMapping[interfaceType] = className;

#if DEBUG
                if (isVerboseLogging)
                    LogToFileHelper.WriteLog($"Add source code:\n /* {sourceCode} */");
#endif            
                context.AddSource($"{className}.cs", sourceCode.ToString());
            }

            // add intercept registration helper class
            context.AddSource($"{InterceptProviderName}.cs", BuildInterceptProvider(interfaceImplementationMapping, targetAssemblyName));


#if DEBUG
            context.AddSource($"SourceGenLogging.cs", LogToFileHelper.GetLogsCodeText());
#endif
        }


        string BuildInterceptProvider(Dictionary<ITypeSymbol, string> interfaceImplementationMapping, string targetNamespace)
        {
            StringBuilder sb = new StringBuilder();
            sb.Append($@"using System;
using System.Collections.Generic;
using System.Text;

namespace {targetNamespace}
{{
    public static class {InterceptProviderName}<InterfaceType>
        where InterfaceType : class
    {{
        static Type? interceptImplementationType = null;

        static {InterceptProviderName}() 
        {{
            // Intercept implementation count: {interfaceImplementationMapping.Count}
");
            foreach (var item in interfaceImplementationMapping)
            {
                sb.Append(MethodLineIntention);
                sb.Append($"{InterceptProviderName}<{item.Key.ToString()}>.interceptImplementationType = typeof(");
                sb.Append(item.Value);
                sb.AppendLine(");");
            }

            sb.AppendLine("        }");



            sb.Append(@"        public static Type GetInterceptType()
        {
            if (interceptImplementationType is null)
                throw new InvalidOperationException($""No auto generated intecept interface implementation assigned! Source generator did not execute properly? Epected implementation interface: {typeof(InterfaceType).FullName}"");

            return interceptImplementationType;
        }
    }
}
");

            return sb.ToString();
        }


        private bool CollectInterceptProviderCalls(SyntaxNode syntaxNode, CancellationToken cancellationToken)
        {
#if DEBUG
            if (isVerboseLogging)
                LogToFileHelper.WriteLog(syntaxNode.GetType().Name);
            //LogToFileHelper.WriteLog(syntaxNode.GetType().Name + ": " + syntaxNode.ToString());
#endif

            if (syntaxNode is IdentifierNameSyntax identifierNameSyntax
                && identifierNameSyntax.Parent is TypeArgumentListSyntax typeArgList
                && typeArgList.Parent is GenericNameSyntax genericName)
            {
                string name = genericName.ToString();

                if (name.StartsWith(InterceptProviderName))
                {
                    // ioc talk local session service implementation
                    return true;
                }
            }


            return false;
        }


        private ITypeSymbol? GetInterceptInterfaceSymbol(GeneratorSyntaxContext context, CancellationToken cancellationToken)
        {
#if DEBUG
            if (isVerboseLogging)
                LogToFileHelper.WriteLog($"Check service interface invoke expression: {context.Node}");
#endif

            if (context.Node is IdentifierNameSyntax nameSyntax)
            {
                var symbolInfo = context.SemanticModel.GetSymbolInfo(nameSyntax, cancellationToken);

                if (symbolInfo.Symbol is not null)
                    return (ITypeSymbol)symbolInfo.Symbol;
            }

            return null;
        }










        public StringBuilder BuildInterfaceInterceptImplementationSource(ITypeSymbol interfaceType, string targetNamespace, out string className)
        {
            if (!interfaceType.IsAbstract)
                throw new Exception("Type must be an interface!");

            className = GetImplName(interfaceType.Name) + InterceptProviderShortName + "AutoGen";

            StringBuilder source = new StringBuilder();

            source.AppendLine("using System;");
            source.AppendLine("using System.Collections.Generic;");
            source.AppendLine("using System.Threading;");
            source.AppendLine($"using {interfaceType.ContainingNamespace};");

            AppendInterceptUsings(source, interfaceType);

            source.AppendLine();
            source.Append("namespace ");
            source.AppendLine(targetNamespace);
            source.AppendLine("{");
            source.Append($" public class {className} : {interfaceType.Name}, IDisposable");

            AppendInterceptInheritance(source, interfaceType);

            source.AppendLine();
            source.AppendLine(" {");

            // add communication service field
            source.AppendLine();
            source.AppendLine($"{MethodBodyIntention}private {interfaceType.Name} {InterceptedServiceFieldName};");
            source.AppendLine($"{MethodBodyIntention}private ICallMonitoringHost {MonitorHostFieldName};");

            AppendInterceptFieldDefinition(source, interfaceType);

            source.AppendLine();

            // add constructor
            source.Append($"{MethodBodyIntention}public {className}({interfaceType.Name} {InterceptedServiceFieldName}");

            AppendInterceptConstructorParameter(source, interfaceType);

            source.AppendLine(")");
            source.AppendLine($"{MethodBodyIntention}{{");
            source.AppendLine($"{MethodLineIntention}this.{InterceptedServiceFieldName} = {InterceptedServiceFieldName};");
            source.AppendLine($"{MethodLineIntention}this.{MonitorHostFieldName} = {MonitorHostFieldName};");

            AppendInterceptConstructorImplementation(source, interfaceType);

            source.AppendLine($"{MethodBodyIntention}}}");
            source.AppendLine();

            source.AppendLine($"{MethodBodyIntention}public object InterceptedServiceObject => {InterceptedServiceFieldName};");
            source.AppendLine();

            var methods = CreateInterceptProviderInterfaceMethodSourceCode(source, interfaceType);

            AppendInterceptCodeAfterMethods(source, interfaceType, methods);

            // Add dispose method
            source.AppendLine();
            source.AppendLine($"{MethodBodyIntention}public void Dispose()");
            source.AppendLine($"{MethodBodyIntention}{{");
            source.AppendLine($"{MethodLineIntention}if ({InterceptedServiceFieldName} is IDisposable interceptedDisposable)");
            source.AppendLine($"{MethodLineIntention}{{");
            source.AppendLine($"{MethodLineIntention}   interceptedDisposable.Dispose();");
            source.AppendLine($"{MethodLineIntention}}}");
            source.AppendLine($"{MethodLineIntention}monitorHost.UnregisterSource(this);");
            AppendInterceptDisposeImplementation(source, interfaceType);
            source.AppendLine($"{MethodBodyIntention}}}");

            source.AppendLine(" }");
            source.AppendLine("}");
            return source;
        }

        private static string GetImplName(string interfaceName)
        {
            if (interfaceName.StartsWith("I"))
            {
                return interfaceName.Substring(1);
            }
            else
            {
                return interfaceName;
            }
        }


        /// <summary>
        /// Determines if the given type is an async await Task like type
        /// </summary>
        /// <param name="type"></param>
        /// <returns></returns>
        public static bool IsAsyncAwaitType(ITypeSymbol type, out bool containsResultValue)
        {

            bool isAsyncAwait = type.AllInterfaces.Where(nts => nts.Name == "IAsyncResult" && nts.ContainingNamespace.Name == "System").Any();

            if (isAsyncAwait)
            {
                bool isGenericType = ((INamedTypeSymbol)type).IsGenericType;

                containsResultValue = isGenericType;
            }
            else
                containsResultValue = false;

            return isAsyncAwait;
        }

        private static string GetSourceCodeTypeName(ITypeSymbol type)
        {
            if (type is IArrayTypeSymbol arrayType)
            {
                return $"{GetSourceNamespace(arrayType.ElementType.ContainingNamespace)}{arrayType.ElementType.Name}[]";
            }
            else if (type is INamedTypeSymbol nts
                && nts.IsGenericType)
            {
                return $"{GetSourceNamespace(type.ContainingNamespace)}{type.Name}<{string.Join(", ", nts.TypeArguments.Select(ta => GetSourceCodeTypeName(ta)))}>";
            }
            else
                return $"{GetSourceNamespace(type.ContainingNamespace)}{type.Name}";
        }

        private static string GetSourceNamespace(INamespaceSymbol namespaceSymbol)
        {
            if (namespaceSymbol.Name == "System" || namespaceSymbol.IsGlobalNamespace)
                return string.Empty;
            else
                return $"{namespaceSymbol}.";
        }


        public static string GetUniqueMethodName(IMethodSymbol method) => $"{method.Name}_{Math.Abs(string.Join("_", method.Parameters.Select(p => p.Type.ToString())).GetHashCode())}";


        public static string FirstToLower(string input)
        {
            if (input.Length > 0
                && char.IsUpper(input[0]))
                input = char.ToLower(input[0]) + input.Substring(1);

            return input;
        }


        public static string GetMethodSignatureWithoutNamespace(IMethodSymbol methodSymbol)
        {
            // Start with the method name
            StringBuilder signatureBuilder = new StringBuilder(methodSymbol.Name + "(");

            // Append each parameter type
            for (int i = 0; i < methodSymbol.Parameters.Length; i++)
            {
                var parameter = methodSymbol.Parameters[i];
                signatureBuilder.Append(parameter.Type.ToDisplayString());
                signatureBuilder.Append(" ");
                signatureBuilder.Append(parameter.Name);

                // Add a comma if it's not the last parameter
                if (i < methodSymbol.Parameters.Length - 1)
                {
                    signatureBuilder.Append(", ");
                }
            }

            signatureBuilder.Append(")");

            return signatureBuilder.ToString();
        }



        private List<IMethodSymbol> CreateInterceptProviderInterfaceMethodSourceCode(StringBuilder mainSource, ITypeSymbol interfaceType)
        {
            List<IMethodSymbol> methods = new List<IMethodSymbol>();

            CreateInterceptInterfaceMethodForTyp(mainSource, interfaceType, methods);

            foreach (var baseInterface in interfaceType.AllInterfaces)
            {
                CreateInterceptInterfaceMethodForTyp(mainSource, baseInterface, methods);
            }

            return methods;
        }

        private void CreateInterceptInterfaceMethodForTyp(StringBuilder mainSource, ITypeSymbol interfaceType, List<IMethodSymbol> methods)
        {
            foreach (var member in interfaceType.GetMembers())
            {
                if (member is IMethodSymbol method)
                {
                    bool methodCallingSignatureAlreadyAdded = methods.Any(m => m.Name == method.Name
                                                        && m.Parameters.Length == method.Parameters.Length
                                                        && m.Parameters.SequenceEqual(method.Parameters, (p1, p2) => SymbolEqualityComparer.Default.Equals(p1.Type, p2.Type)));

                    if (methodCallingSignatureAlreadyAdded == false)
                    {
                        CreateInterceptInterfaceMemberMethod(mainSource, interfaceType, method, false);
                        methods.Add(method);
                    }
                }
            }
        }

        private void CreateInterceptInterfaceMemberMethod(StringBuilder mainSource, ITypeSymbol interfaceType, IMethodSymbol method, bool isExplicitImplementation)
        {
            StringBuilder methodSource = new StringBuilder();


            bool isReturnRequired;
            string returnType;
            bool isAsyncAwait = false;
            bool containsAsyncResultValue = false;
            if (method.ReturnsVoid)
            {
                isReturnRequired = false;
                returnType = "void";
            }
            else
            {
                isReturnRequired = true;
                isAsyncAwait = IsAsyncAwaitType(method.ReturnType, out containsAsyncResultValue);
                if (isAsyncAwait)
                {
                    isReturnRequired = containsAsyncResultValue;

                }

                returnType = GetSourceCodeTypeName(method.ReturnType);
            }

            AppendInterceptMethodPreDefinition(methodSource, interfaceType, method);

            string methodName = isExplicitImplementation ? $"{interfaceType}.{method.Name}" : method.Name;
            string publicPrefix = isExplicitImplementation ? "" : "public";

            if (isAsyncAwait)
            {
                methodSource.Append($"{MethodBodyIntention}{publicPrefix} async {returnType} {methodName}(");
            }
            else
                methodSource.AppendFormat($"{MethodBodyIntention}{publicPrefix} {returnType} {methodName}(");



            List<IParameterSymbol>? outParameters = null;
            StringBuilder? sbParameterValues = null;

            var parameters = method.Parameters;



            if (parameters.Length > 0)
            {
                sbParameterValues = new StringBuilder();

                for (int i = 0; i < parameters.Length; i++)
                {
                    var param = parameters[i];

                    ITypeSymbol paramType = param.Type;
                    string? decoration = null;
                    //if (param.IsOut)
                    if (param.RefKind == RefKind.Out)
                    {
                        decoration = "out ";
                        //paramType = paramType.GetElementType();

                        if (outParameters == null)
                            outParameters = new List<IParameterSymbol>();

                        outParameters.Add(param);
                    }

                    string parameterTypeString = GetSourceCodeTypeName(paramType);

                    methodSource.Append($"{decoration}{parameterTypeString} {param.Name}");

                    //string invokeInfoMemberTypeName = string.Concat(decoration, parameterTypeString);
                    //invokeInfoMemberTypeName = Regex.Replace(invokeInfoMemberTypeName, @"[^a-zA-Z0-9]", "");    // remove invalid chars

                    //if (paramType is IArrayTypeSymbol)
                    //    invokeInfoMemberTypeName += "Arr";

                    //invokeInfoMemberName += invokeInfoMemberTypeName;

                    // add reference to parameter value array
                    if (param.RefKind == RefKind.Out)
                    {
                        // out parameter -> pass null
                        sbParameterValues.Append($"out {param.Name}");
                    }
                    else
                    {
                        sbParameterValues.Append(param.Name);
                    }

                    // type array
                    //sbParameterTypes.AppendFormat("typeof({0}){1}", parameterTypeString, (param.RefKind == RefKind.Out ? ".MakeByRefType()" : null));


                    if (i < parameters.Length - 1)
                    {
                        methodSource.Append(", ");
                        sbParameterValues.Append(", ");
                    }
                }
            }



            methodSource.AppendLine(")");

            methodSource.Append(MethodBodyIntention);
            methodSource.AppendLine("{");

            if (isReturnRequired)
            {
                methodSource.Append(MethodLineIntention);
                if (isAsyncAwait)
                    methodSource.Append(GetSourceCodeTypeName(((INamedTypeSymbol)method.ReturnType).TypeArguments[0]));     // unwind Task<T>
                else
                    methodSource.Append(GetSourceCodeTypeName(method.ReturnType));
                methodSource.AppendLine(" result = default;");
            }

            AppendInterceptMethodBeforeNestedCall(methodSource, interfaceType, method);

            methodSource.Append(MethodLineIntention2);

            if (isReturnRequired)
                methodSource.Append($"result = ");

            if (isAsyncAwait)
                methodSource.Append("await ");


            methodSource.Append($"{InterceptedServiceFieldName}.{method.Name}(");
            if (sbParameterValues != null)
                methodSource.Append(sbParameterValues);
            methodSource.AppendLine(");");


            //if (outParameters != null)
            //{
            //    // assign out parameters

            //    for (int outParamIndex = 0; outParamIndex < outParameters.Count; outParamIndex++)
            //    {
            //        var outParam = outParameters[outParamIndex];
            //        methodSource.AppendFormat("{0}{1} = ({2})parameterValues[{3}];", MethodLineIntention, outParam.Name, GetSourceCodeTypeName(outParam.Type), outParamIndex);
            //        methodSource.AppendLine();
            //    }
            //}

            AppendInterceptMethodAfterNestedCall(methodSource, interfaceType, method);

            if (isReturnRequired)
            {
                if (isAsyncAwait)
                {
                    if (containsAsyncResultValue)
                    {
                        //string genericTypeStr = GetSourceCodeTypeName(((INamedTypeSymbol)method.ReturnType).TypeArguments.First());

                        methodSource.Append($"{MethodLineIntention}return result;");
                    }
                    // else: Task/ValueTask void return
                }
                else
                    methodSource.AppendFormat($"{MethodLineIntention}return result;");

                methodSource.AppendLine();
            }

            methodSource.Append(MethodBodyIntention);
            methodSource.AppendLine("}");
            methodSource.AppendLine();

            AppendInterceptMethodPostDefinition(methodSource, interfaceType, method);

            // add to main source
            mainSource.Append(methodSource);
        }




        protected virtual void AppendInterceptMethodPreDefinition(StringBuilder methodSource, ITypeSymbol interfaceType, IMethodSymbol method)
        {
        }

        protected virtual void AppendInterceptMethodBeforeNestedCall(StringBuilder methodSource, ITypeSymbol interfaceType, IMethodSymbol method)
        {
        }

        protected virtual void AppendInterceptMethodAfterNestedCall(StringBuilder methodSource, ITypeSymbol interfaceType, IMethodSymbol method)
        {
        }

        protected virtual void AppendInterceptMethodPostDefinition(StringBuilder methodSource, ITypeSymbol interfaceType, IMethodSymbol method)
        {
        }

        protected virtual void AppendInterceptCodeAfterMethods(StringBuilder source, ITypeSymbol interfaceType, List<IMethodSymbol> methods)
        {
        }

        protected virtual void AppendInterceptInheritance(StringBuilder source, ITypeSymbol interfaceType)
        {
        }

        protected virtual void AppendInterceptUsings(StringBuilder source, ITypeSymbol interfaceType)
        {
        }

        protected virtual void AppendInterceptConstructorParameter(StringBuilder source, ITypeSymbol interfaceType)
        {
        }

        protected virtual void AppendInterceptConstructorImplementation(StringBuilder source, ITypeSymbol interfaceType)
        {
        }

        protected virtual void AppendInterceptFieldDefinition(StringBuilder source, ITypeSymbol interfaceType)
        {
        }


        protected virtual void AppendInterceptDisposeImplementation(StringBuilder source, ITypeSymbol interfaceType)
        {
        }
    }
}
