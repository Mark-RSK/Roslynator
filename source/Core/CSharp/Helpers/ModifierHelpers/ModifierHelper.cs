// Copyright (c) Josef Pihrt. All rights reserved. Licensed under the Apache License, Version 2.0. See License.txt in the project root for license information.

using System;
using System.Diagnostics;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Roslynator.CSharp.Comparers;
using static Microsoft.CodeAnalysis.CSharp.SyntaxFactory;

namespace Roslynator.CSharp.Helpers.ModifierHelpers
{
    internal static class ModifierHelper
    {
        public static TNode InsertModifier<TNode>(TNode node, SyntaxKind modifierKind, IModifierComparer comparer = null) where TNode : SyntaxNode
        {
            return InsertModifier(node, Token(modifierKind), comparer);
        }

        public static TNode InsertModifier<TNode>(TNode node, SyntaxToken modifier, IModifierComparer comparer = null) where TNode : SyntaxNode
        {
            if (node == null)
                throw new ArgumentNullException(nameof(node));

            SyntaxTokenList modifiers = node.GetModifiers();

            Debug.Assert(modifiers.Any() || modifiers == default(SyntaxTokenList), node.ToString());

            if (!modifiers.Any())
            {
                SyntaxNodeOrToken nodeOrToken = FindNodeOrTokenAfterModifiers(node);

                if (!nodeOrToken.IsKind(SyntaxKind.None))
                {
                    SyntaxTriviaList trivia = nodeOrToken.GetLeadingTrivia();

                    if (trivia.Any())
                    {
                        SyntaxTriviaList leadingTrivia = modifier.LeadingTrivia;

                        if (!leadingTrivia.IsSingleElasticMarker())
                            trivia = trivia.AddRange(leadingTrivia);

                        if (nodeOrToken.IsNode)
                        {
                            SyntaxNode node2 = nodeOrToken.AsNode();
                            node = node.ReplaceNode(node2, node2.WithoutLeadingTrivia());
                        }
                        else
                        {
                            SyntaxToken token = nodeOrToken.AsToken();
                            node = node.ReplaceToken(token, token.WithoutLeadingTrivia());
                        }

                        return node.WithModifiers(TokenList(modifier.WithLeadingTrivia(trivia)));
                    }
                }
            }

            return node.WithModifiers(modifiers.InsertModifier(modifier, comparer ?? ModifierComparer.Instance));
        }

        public static TNode RemoveModifier<TNode>(TNode node, SyntaxKind modifierKind) where TNode : SyntaxNode
        {
            if (node == null)
                throw new ArgumentNullException(nameof(node));

            SyntaxTokenList modifiers = node.GetModifiers();

            int i = modifiers.IndexOf(modifierKind);

            if (i != -1)
            {
                return RemoveModifier(node, modifiers, modifiers[i], i);
            }
            else
            {
                return node;
            }
        }

        public static TNode RemoveModifier<TNode>(TNode node, SyntaxToken modifier) where TNode : SyntaxNode
        {
            if (node == null)
                throw new ArgumentNullException(nameof(node));

            SyntaxTokenList modifiers = node.GetModifiers();

            int i = modifiers.IndexOf(modifier);

            if (i != -1)
            {
                return RemoveModifier(node, modifiers, modifier, i);
            }
            else
            {
                return node;
            }
        }

        public static TNode RemoveModifierAt<TNode>(TNode node, int index) where TNode : SyntaxNode
        {
            if (node == null)
                throw new ArgumentNullException(nameof(node));

            SyntaxTokenList modifiers = node.GetModifiers();

            return RemoveModifier(node, modifiers, modifiers[index], index);
        }

        private static TNode RemoveModifier<TNode>(
            TNode node,
            SyntaxTokenList modifiers,
            SyntaxToken modifier,
            int index) where TNode : SyntaxNode
        {
            SyntaxTriviaList leading = modifier.LeadingTrivia;
            SyntaxTriviaList trailing = modifier.TrailingTrivia;

            if (modifiers.Count == 1)
            {
                SyntaxToken nextToken = modifier.GetNextToken();

                if (!nextToken.IsKind(SyntaxKind.None))
                {
                    SyntaxTriviaList trivia = leading.AddIfNotEmptyOrWhitespace(trailing, nextToken.LeadingTrivia);

                    node = node.ReplaceToken(nextToken, nextToken.WithLeadingTrivia(trivia));
                }
                else
                {
                    SyntaxToken previousToken = modifier.GetPreviousToken();

                    if (!previousToken.IsKind(SyntaxKind.None))
                    {
                        SyntaxTriviaList trivia = previousToken.TrailingTrivia.AddIfNotEmptyOrWhitespace(leading, trailing);

                        node = node.ReplaceToken(previousToken, previousToken.WithTrailingTrivia(trivia));
                    }
                }
            }
            else
            {
                if (index == 0)
                {
                    SyntaxToken nextModifier = modifiers[index + 1];

                    SyntaxTriviaList trivia = leading.AddIfNotEmptyOrWhitespace(trailing, nextModifier.LeadingTrivia);

                    modifiers = modifiers.Replace(nextModifier, nextModifier.WithLeadingTrivia(trivia));
                }
                else
                {
                    SyntaxToken previousModifier = modifiers[index - 1];

                    SyntaxTriviaList trivia = previousModifier.TrailingTrivia.AddIfNotEmptyOrWhitespace(leading, trailing);

                    modifiers = modifiers.Replace(previousModifier, previousModifier.WithTrailingTrivia(trivia));
                }
            }

            modifiers = modifiers.RemoveAt(index);

            return node.WithModifiers(modifiers);
        }

        private static SyntaxTriviaList AddIfNotEmptyOrWhitespace(this SyntaxTriviaList trivia, SyntaxTriviaList triviaToAdd)
        {
            return (triviaToAdd.IsEmptyOrWhitespace()) ? trivia : trivia.AddRange(triviaToAdd);
        }

        private static SyntaxTriviaList AddIfNotEmptyOrWhitespace(this SyntaxTriviaList trivia, SyntaxTriviaList triviaToAdd1, SyntaxTriviaList triviaToAdd2)
        {
            return trivia
                .AddIfNotEmptyOrWhitespace(triviaToAdd1)
                .AddIfNotEmptyOrWhitespace(triviaToAdd2);
        }

        private static SyntaxNodeOrToken FindNodeOrTokenAfterModifiers(SyntaxNode node)
        {
            switch (node.Kind())
            {
                case SyntaxKind.ClassDeclaration:
                    return ((ClassDeclarationSyntax)node).Identifier;
                case SyntaxKind.ConstructorDeclaration:
                    return ((ConstructorDeclarationSyntax)node).Identifier;
                case SyntaxKind.ConversionOperatorDeclaration:
                    return ((ConversionOperatorDeclarationSyntax)node).ImplicitOrExplicitKeyword;
                case SyntaxKind.DelegateDeclaration:
                    return ((DelegateDeclarationSyntax)node).DelegateKeyword;
                case SyntaxKind.DestructorDeclaration:
                    return ((DestructorDeclarationSyntax)node).TildeToken;
                case SyntaxKind.EnumDeclaration:
                    return ((EnumDeclarationSyntax)node).EnumKeyword;
                case SyntaxKind.EventDeclaration:
                    return ((EventDeclarationSyntax)node).EventKeyword;
                case SyntaxKind.EventFieldDeclaration:
                    return ((EventFieldDeclarationSyntax)node).EventKeyword;
                case SyntaxKind.FieldDeclaration:
                    return ((FieldDeclarationSyntax)node).Declaration?.Type;
                case SyntaxKind.IndexerDeclaration:
                    return ((IndexerDeclarationSyntax)node).Type;
                case SyntaxKind.InterfaceDeclaration:
                    return ((InterfaceDeclarationSyntax)node).Keyword;
                case SyntaxKind.MethodDeclaration:
                    return ((MethodDeclarationSyntax)node).ReturnType;
                case SyntaxKind.OperatorDeclaration:
                    return ((OperatorDeclarationSyntax)node).ReturnType;
                case SyntaxKind.PropertyDeclaration:
                    return ((PropertyDeclarationSyntax)node).Type;
                case SyntaxKind.StructDeclaration:
                    return ((StructDeclarationSyntax)node).Keyword;
                case SyntaxKind.GetAccessorDeclaration:
                case SyntaxKind.SetAccessorDeclaration:
                case SyntaxKind.AddAccessorDeclaration:
                case SyntaxKind.RemoveAccessorDeclaration:
                case SyntaxKind.UnknownAccessorDeclaration:
                    return ((AccessorDeclarationSyntax)node).Keyword;
                case SyntaxKind.LocalDeclarationStatement:
                    return ((LocalDeclarationStatementSyntax)node).Declaration?.Type;
                case SyntaxKind.LocalFunctionStatement:
                    return ((LocalFunctionStatementSyntax)node).ReturnType;
                case SyntaxKind.Parameter:
                    return ((ParameterSyntax)node).Type;
                default:
                    {
                        Debug.Assert(node.IsKind(SyntaxKind.IncompleteMember), node.ToString());
                        return default(SyntaxNodeOrToken);
                    }
            }
        }

        public static TNode RemoveAccessModifiers<TNode>(TNode node) where TNode : SyntaxNode
        {
            SyntaxTokenList modifiers = node.GetModifiers();

            for (int i = modifiers.Count - 1; i >= 0; i--)
            {
                SyntaxToken modifier = modifiers[i];

                if (modifier.IsAccessModifier())
                    node = RemoveModifier(node, modifiers, modifier, i);
            }

            return node;
        }
    }
}
