export default class Mailbox {
  private worker: Worker;
  private requestMap: Map<number, ((response: any) => void)> = new Map();
  private nextRequestId = 0;
  constructor(worker: Worker) {
    this.worker = worker;
    worker.addEventListener('message', (event) => {
      const callback = this.requestMap.get(event.data.id);
      if (!callback) {
        throw new Error("unknown request", event.data);
      }
      this.requestMap.delete(event.data.id);
      callback(event.data.response);
    });
  }

  send(request : any) {
    return new Promise((resolve) => {
      const id = this.nextRequestId++;
      this.requestMap.set(id, resolve);
      this.worker.postMessage({id: id, request: request});
    });
  }
}
