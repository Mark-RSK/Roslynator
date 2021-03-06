﻿// Copyright (c) Josef Pihrt. All rights reserved. Licensed under the Apache License, Version 2.0. See License.txt in the project root for license information.

using System.Collections.Immutable;
using System.Composition;
using System.Diagnostics;
using System.Threading;
using System.Threading.Tasks;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CodeActions;
using Microsoft.CodeAnalysis.CodeFixes;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;

namespace Roslynator.CSharp.CodeFixes
{
    [ExportCodeFixProvider(LanguageNames.CSharp, Name = nameof(ChangeTypeOfLocalVariableCodeFixProvider))]
    [Shared]
    public class ChangeTypeOfLocalVariableCodeFixProvider : BaseCodeFixProvider
    {
        public sealed override ImmutableArray<string> FixableDiagnosticIds
        {
            get
            {
                return ImmutableArray.Create(
                    CompilerDiagnosticIdentifiers.CannotAssignMethodGroupToImplicitlyTypedVariable,
                    CompilerDiagnosticIdentifiers.NoOverloadMatchesDelegate,
                    CompilerDiagnosticIdentifiers.MethodHasWrongReturnType);
            }
        }

        public override FixAllProvider GetFixAllProvider()
        {
            return null;
        }

        public sealed override async Task RegisterCodeFixesAsync(CodeFixContext context)
        {
            if (!Settings.IsCodeFixEnabled(CodeFixIdentifiers.ChangeTypeOfLocalVariable))
                return;

            SyntaxNode root = await context.GetSyntaxRootAsync().ConfigureAwait(false);

            VariableDeclaratorSyntax variableDeclarator = root
                .FindNode(context.Span, getInnermostNodeForTie: true)?
                .FirstAncestorOrSelf<VariableDeclaratorSyntax>();

            Debug.Assert(variableDeclarator != null, $"{nameof(variableDeclarator)} is null");

            if (variableDeclarator == null)
                return;

            foreach (Diagnostic diagnostic in context.Diagnostics)
            {
                switch (diagnostic.Id)
                {
                    case CompilerDiagnosticIdentifiers.CannotAssignMethodGroupToImplicitlyTypedVariable:
                    case CompilerDiagnosticIdentifiers.NoOverloadMatchesDelegate:
                    case CompilerDiagnosticIdentifiers.MethodHasWrongReturnType:
                        {
                            if (!variableDeclarator.IsParentKind(SyntaxKind.VariableDeclaration))
                                break;

                            var variableDeclaration = (VariableDeclarationSyntax)variableDeclarator.Parent;

                            ExpressionSyntax value = variableDeclarator.Initializer?.Value;

                            if (value == null)
                                break;

                            SemanticModel semanticModel = await context.GetSemanticModelAsync().ConfigureAwait(false);

                            SymbolInfo symbolInfo = semanticModel.GetSymbolInfo(value, context.CancellationToken);

                            if (symbolInfo.Symbol != null)
                            {
                                ComputeCodeFix(context, diagnostic, variableDeclarator, symbolInfo.Symbol, semanticModel);
                            }
                            else
                            {
                                foreach (ISymbol candidateSymbol in symbolInfo.CandidateSymbols)
                                    ComputeCodeFix(context, diagnostic, variableDeclarator, candidateSymbol, semanticModel);
                            }

                            break;
                        }
                }
            }
        }

        private void ComputeCodeFix(
            CodeFixContext context,
            Diagnostic diagnostic,
            VariableDeclaratorSyntax variableDeclarator,
            ISymbol symbol,
            SemanticModel semanticModel)
        {
            var methodSymbol = symbol as IMethodSymbol;

            if (methodSymbol != null)
            {
                ImmutableArray<IParameterSymbol> parameters = methodSymbol.Parameters;

                if (parameters.Length <= 16)
                {
                    ITypeSymbol returnType = methodSymbol.ReturnType;

                    if (SupportsExplicitDeclaration(returnType, parameters))
                    {
                        INamedTypeSymbol typeSymbol = ConstructActionOrFunc(returnType, parameters, semanticModel);

                        CodeAction codeAction = CodeAction.Create(
                            $"Change type to '{SymbolDisplay.GetMinimalString(typeSymbol, semanticModel, variableDeclarator.SpanStart)}'",
                            cancellationToken => RefactorAsync(context.Document, (VariableDeclarationSyntax)variableDeclarator.Parent, typeSymbol, semanticModel, cancellationToken),
                            GetEquivalenceKey(diagnostic, SymbolDisplay.GetString(typeSymbol)));

                        context.RegisterCodeFix(codeAction, diagnostic);
                    }
                }
            }
        }

        private static bool SupportsExplicitDeclaration(ITypeSymbol returnType, ImmutableArray<IParameterSymbol> parameters)
        {
            if (!returnType.IsVoid()
                && !returnType.SupportsExplicitDeclaration())
            {
                return false;
            }

            foreach (IParameterSymbol parameter in parameters)
            {
                if (!parameter.Type.SupportsExplicitDeclaration())
                    return false;
            }

            return true;
        }

        private static INamedTypeSymbol ConstructActionOrFunc(
            ITypeSymbol returnType,
            ImmutableArray<IParameterSymbol> parameters,
            SemanticModel semanticModel)
        {
            int length = parameters.Length;

            if (returnType.IsVoid())
            {
                if (length == 0)
                    return semanticModel.GetTypeByMetadataName("System.Action");

                INamedTypeSymbol actionSymbol = semanticModel.GetTypeByMetadataName($"System.Action`{length.ToString()}");

                var typeArguments = new ITypeSymbol[length];

                for (int i = 0; i < length; i++)
                    typeArguments[i] = parameters[i].Type;

                return actionSymbol.Construct(typeArguments);
            }
            else
            {
                INamedTypeSymbol funcSymbol = semanticModel.GetTypeByMetadataName($"System.Func`{(length + 1).ToString()}");

                var typeArguments = new ITypeSymbol[length + 1];

                for (int i = 0; i < length; i++)
                    typeArguments[i] = parameters[i].Type;

                typeArguments[length] = returnType;

                return funcSymbol.Construct(typeArguments);
            }
        }

        private static Task<Document> RefactorAsync(
            Document document,
            VariableDeclarationSyntax variableDeclaration,
            INamedTypeSymbol typeSymbol,
            SemanticModel semanticModel,
            CancellationToken cancellationToken)
        {
            TypeSyntax type = typeSymbol.ToMinimalTypeSyntax(semanticModel, variableDeclaration.SpanStart);

            VariableDeclarationSyntax newNode = variableDeclaration.WithType(type.WithTriviaFrom(variableDeclaration.Type));

            return document.ReplaceNodeAsync(variableDeclaration, newNode, cancellationToken);
        }
    }
}
