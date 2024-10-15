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
  hideAll: hideAll,
};

CodeMirror.commands = Object.assign(CodeMirror.commands, commands);
