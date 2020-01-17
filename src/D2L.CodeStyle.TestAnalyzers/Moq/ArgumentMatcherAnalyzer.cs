using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Text;
using D2L.CodeStyle.TestAnalyzers.Common;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Diagnostics;

namespace D2L.CodeStyle.TestAnalyzers.Moq {

	[DiagnosticAnalyzer( LanguageNames.CSharp )]
	internal sealed class ArgumentMatcherAnalyzer : DiagnosticAnalyzer {

		private const string MoqItFullName = "Moq.It";

		private static readonly ImmutableArray<string> ItArgumentMatcherMethods = ImmutableArray.Create(
				"Is",
				"IsAny",
				"IsIn",
				"IsInRange",
				"IsNotIn",
				"IsNotNull"
			);

		public override ImmutableArray<DiagnosticDescriptor> SupportedDiagnostics => ImmutableArray.Create(
			Diagnostics.TestMoqArgumentMatcherTypeMismatch
		);

		public override void Initialize( AnalysisContext context ) {

			context.EnableConcurrentExecution();
			context.RegisterCompilationStartAction( RegisterAnalysis );
		}

		private void RegisterAnalysis( CompilationStartAnalysisContext context ) {

			Compilation compilation = context.Compilation;

			INamedTypeSymbol itType = compilation.GetTypeByMetadataName( MoqItFullName );
			if( itType == null ) {
				return;
			}

			IImmutableSet<ISymbol> argumentMatchers = GetItArgumentMatcherMethods( itType, ItArgumentMatcherMethods );

			context.RegisterSyntaxNodeAction(
					ctxt => {
						if( ctxt.Node is InvocationExpressionSyntax invocation ) {
							AnalyzeMethodInvocation( ctxt, invocation, argumentMatchers );
						}
					},
					SyntaxKind.InvocationExpression
				);
		}

		private void AnalyzeMethodInvocation(
				SyntaxNodeAnalysisContext context,
				InvocationExpressionSyntax invocation,
				IImmutableSet<ISymbol> argumentMatchers
			) {

			ISymbol methodSymbol = context.SemanticModel
				.GetSymbolInfo( invocation.Expression )
				.Symbol;

		}

		private static IImmutableSet<ISymbol> GetItArgumentMatcherMethods(
				INamedTypeSymbol type,
				IEnumerable<string> methodNames
			) {

			ImmutableHashSet<ISymbol>.Builder builder = ImmutableHashSet.CreateBuilder<ISymbol>();

			foreach( string name in methodNames ) {

				IEnumerable<ISymbol> methods = type
					.GetMembers( name )
					.Where( m => m.Kind == SymbolKind.Method );

				foreach( ISymbol method in methods ) {
					builder.Add( method );
				}
			}

			return builder.ToImmutableHashSet();
		}
	}
}
