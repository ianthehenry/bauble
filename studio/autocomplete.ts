import {CompletionContext, Completion} from "@codemirror/autocomplete";
import type {Definition} from 'bauble-runtime';
import {JanetLanguage} from 'codemirror-lang-janet';
import {syntaxTree} from '@codemirror/language';
import type {SyntaxNode} from '@lezer/common';

import {unified} from 'unified';
import remarkParse from 'remark-parse';
import remarkRehype from 'remark-rehype';
import rehypeSanitize from 'rehype-sanitize';
import rehypeStringify from 'rehype-stringify';

const isFormBoundaryChar = (char: string) => {
  switch (char) {
  case '(': return true;
  case '|': return true;
  case '+': return true;
  case '-': return true;
  case '*': return true;
  case '/': return true;
  }
  return false;
};

const getCompletions = (values: Completion[]) => (context: CompletionContext) => {
  const cursor = syntaxTree(context.state).resolveInner(context.pos, -1).cursor();
  cursor.childBefore(context.pos);
  const node = cursor.node;

  const nodeText = (node: SyntaxNode) => context.state.sliceDoc(node.from, node.to);

  const isInitial = (node: SyntaxNode) => {
    const prev = node.prevSibling;
    return prev != null && isFormBoundaryChar(nodeText(prev));
  };

  const isIdentifier = (node: SyntaxNode) => node.name === 'Identifier';

  if (context.explicit || isInitial(node) && isIdentifier(node)) {
    const from = isIdentifier(node) ? node.from : context.pos;
    return {
      from: from,
      validFor: /^[a-zA-Z0-9!?$&*+\\./:<=>@^_-]*$/,
      options: values,
    };
  } else {
    return null;
  }
};

const renderMarkdown = (markdown: string) => {
  const node = document.createElement('div');
  node.innerHTML = String(unified()
    .use(remarkParse)
    .use(remarkRehype)
    .use(rehypeSanitize)
    .use(rehypeStringify)
    .processSync(markdown)
    .value);
  return node;
};

function renderDefinition(def: Definition): Completion {
  return {
    label: def.name,
    detail: def.args,
    type: def.type == 0 ? 'variable' : def.type == 1 ? 'function' : 'text',
    info: (_) => renderMarkdown(def.doc),
  };
}

export default function janetAutocomplete(definitions: Array<Definition>) {
  return JanetLanguage.data.of({
    autocomplete: getCompletions(definitions.map(renderDefinition)),
  });
}
