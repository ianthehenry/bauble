import {autocompletion, CompletionContext, Completion, completionStatus,
        startCompletion, moveCompletionSelection} from "@codemirror/autocomplete";
import type {Definition, DefinitionVector} from 'bauble-runtime';
import {JanetLanguage} from 'codemirror-lang-janet';
import {syntaxTree} from '@codemirror/language';

import {unified} from 'unified'
import remarkParse from 'remark-parse'
import remarkRehype from 'remark-rehype'
import rehypeSanitize from 'rehype-sanitize'
import rehypeStringify from 'rehype-stringify'

const getCompletions = (values: Completion[]) => (context: CompletionContext) => {
  const node = syntaxTree(context.state).resolveInner(context.pos, -1);
  const word = context.state.sliceDoc(node.from, context.pos);
  const isHead =
    node.name === 'Identifier' &&
    node.prevSibling != null &&
    node.prevSibling.name === '(';

  if (context.explicit || (isHead && word !== '')) {
    const from = node.name === 'Identifier' ? node.from : context.pos;
    return {
      from: from,
      validFor: /^[a-zA-Z0-9!?$&*+\\./:<=>@^_-]*$/,
      options: values,
    };
  } else {
    return null;
  }
}

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
}

function renderDefinition(def: Definition): Completion {
  return {
    label: def.name,
    detail: def.args,
    type: def.type == 0 ? 'variable' : def.type == 1 ? 'function' : 'text',
    info: (_) => renderMarkdown(def.doc),
  }
}

function renderDefinitions(defs: DefinitionVector) {
  const rendered = [];
  for (let i = 0; i < defs.size(); i++) {
    rendered.push(renderDefinition(defs.get(i)));
  }
  defs.delete();
  return rendered;
}

export default function janetAutocomplete(definitions: DefinitionVector) {
  return JanetLanguage.data.of({
    autocomplete: getCompletions(renderDefinitions(definitions))
  });
}
