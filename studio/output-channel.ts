export default class OutputChannel {
  private _target: HTMLElement | null = null;

  set target(value: HTMLElement | null) {
    this._target = value;
  }

  print(text: string, isErr: boolean) {
    if (this._target == null) {
      if (isErr) {
        console.error(text);
      } else {
        console.log(text);
      }
    } else {
      const span = document.createElement('span');
      span.classList.toggle('err', isErr);
      span.appendChild(document.createTextNode(text));
      span.appendChild(document.createTextNode('\n'));
      this._target.appendChild(span);
    }
  }
}
