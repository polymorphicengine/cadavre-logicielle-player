const commands = {
  evalBlockAtCursor: evalBlockAtCursor,
  evalLineAtCursor: evalLineAtCursor,
  saveFile: saveFile,
  loadFile: loadFile,
  increaseFontSize: increaseFontSize,
  decreaseFontSize: decreaseFontSize,
  swapLineUp: swapLineUp,
  swapLineDown: swapLineDown,
  duplicateLine: duplicateLine,
};

CodeMirror.commands = Object.assign(CodeMirror.commands, commands);
