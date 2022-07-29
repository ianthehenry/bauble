(function() {

function clear() {
  const output = document.getElementById('output');
  output.innerHTML = "";
}

function print(text, isErr=false) {
  const output = document.getElementById('output');
  const span = document.createElement('span');
  span.classList.toggle('err', isErr);
  span.appendChild(document.createTextNode(text));
  span.appendChild(document.createTextNode('\n'));
  output.appendChild(span);
  output.scrollTop = output.scrollHeight;
}

let evaluateJanet = null;
let ready = function() {};

function onReady(f) {
  if (ready == null) {
    f();
  } else {
    const old = ready;
    ready = function() {
      old();
      f();
    };  
  }
}

const preamble = '(use ./shapes)\n'

function executeJanet(code) {
  if (evaluateJanet === null) {
    console.error('not ready yet');
    return;
  }
  const result = evaluateJanet(preamble + code);
  if (result !== 0) {
    print('ERREXIT: ' + result, true);
  }
}

const Module = {
  outputElement: null,
  preRun: [],
  print: function(x) {
    print(x, false);
  },
  printErr: function(x) {
    print(x, true);
  },
  postRun: [function() {
    evaluateJanet = Module.cwrap("run_janet", 'number', ['string']);
    ready();
  }],
};

document.addEventListener("DOMContentLoaded", function (e) {
  const editor = ace.edit('code-editor');

  function runCode() {
    setTimeout(function() {
      clear();
      executeJanet(editor.getValue());
    }, 0);
  }

  editor.session.setMode("ace/mode/clojure");
  editor.setOptions({
    tabSize: 2,
    useSoftTabs: true,
  });
  editor.commands.addCommand({
    name: 'run',
    bindKey: {win: 'Ctrl-Enter',  mac: 'Cmd-Enter'},
    exec: function(editor) {
      runCode();
    },
    readOnly: true
  });
  editor.getSession().on('change', function() {
    runCode();
  });
  onReady(runCode);
  editor.focus();
  editor.navigateFileEnd();
});

window.Module = Module;

})();
