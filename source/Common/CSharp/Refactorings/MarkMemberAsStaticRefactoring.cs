// Copyright (c) Josef Pihrt. All rights reserved. Licensed under the Apache License, Version 2.0. See License.txt in the project root for license information.

using System;
using System.Threading;
using System.Threading.Tasks;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Roslynator.CSharp.Comparers;
using Roslynator.CSharp.Helpers.ModifierHelpers;

namespace Roslynator.CSharp.Refactorings
{
    internal static class MarkMemberAsStaticRefactoring
    {
        public static bool CanRefactor(FieldDeclarationSyntax fieldDeclaration)
        {
            if (fieldDeclaration == null)
                throw new ArgumentNullException(nameof(fieldDeclaration));

            return !fieldDeclaration.IsStatic()
                && !fieldDeclaration.IsConst()
                && IsStaticClass(fieldDeclaration.Parent);
        }

        public static bool CanRefactor(MethodDeclarationSyntax methodDeclaration)
        {
            if (methodDeclaration == null)
                throw new ArgumentNullException(nameof(methodDeclaration));

            return !methodDeclaration.IsStatic()
                && IsStaticClass(methodDeclaration.Parent);
        }

        public static bool CanRefactor(PropertyDeclarationSyntax propertyDeclaration)
        {
            if (propertyDeclaration == null)
                throw new ArgumentNullException(nameof(propertyDeclaration));

            return !propertyDeclaration.IsStatic()
                && IsStaticClass(propertyDeclaration.Parent);
        }

        public static bool CanRefactor(EventDeclarationSyntax eventDeclaration)
        {
            if (eventDeclaration == null)
                throw new ArgumentNullException(nameof(eventDeclaration));

            return !eventDeclaration.IsStatic()
                && IsStaticClass(eventDeclaration.Parent);
        }

        public static bool CanRefactor(EventFieldDeclarationSyntax eventFieldDeclaration)
        {
            if (eventFieldDeclaration == null)
                throw new ArgumentNullException(nameof(eventFieldDeclaration));

            return !eventFieldDeclaration.IsStatic()
                && IsStaticClass(eventFieldDeclaration.Parent);
        }

        public static bool CanRefactor(ConstructorDeclarationSyntax constructorDeclaration)
        {
            if (constructorDeclaration == null)
                throw new ArgumentNullException(nameof(constructorDeclaration));

            return !constructorDeclaration.IsStatic()
                && IsStaticClass(constructorDeclaration.Parent);
        }

        private static bool IsStaticClass(SyntaxNode node)
        {
            return node?.IsKind(SyntaxKind.ClassDeclaration) == true
                && ((ClassDeclarationSyntax)node).IsStatic();
        }

        public static Task<Document> RefactorAsync(
            Document document,
            MemberDeclarationSyntax memberDeclaration,
            CancellationToken cancellationToken = default(CancellationToken))
        {
            if (document == null)
                throw new ArgumentNullException(nameof(document));

            if (memberDeclaration == null)
                throw new ArgumentNullException(nameof(memberDeclaration));

            MemberDeclarationSyntax newMemberDeclaration = memberDeclaration;

            if (memberDeclaration.IsKind(SyntaxKind.ConstructorDeclaration))
                newMemberDeclaration = ModifierHelper.RemoveAccessModifiers(memberDeclaration);

            newMemberDeclaration = newMemberDeclaration.InsertModifier(SyntaxKind.StaticKeyword, ModifierComparer.Instance);

            return document.ReplaceNodeAsync(memberDeclaration, newMemberDeclaration, cancellationToken);
        }
    }
}
