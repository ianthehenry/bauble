export default class Mailbox {
  private worker: Worker;
  private requestMap: Map<number, [(response: any) => void, (error: Error) => void]> = new Map();
  private nextRequestId = 0;
  constructor(worker: Worker) {
    this.worker = worker;
    worker.addEventListener('message', (event) => {
      const callback = this.requestMap.get(event.data.id);
      if (!callback) {
        throw new Error("response to unknown request " + JSON.stringify(event.data));
      }
      this.requestMap.delete(event.data.id);
      const [resolve, reject] = callback;
      if (Object.hasOwn(event.data, 'response')) {
        resolve(event.data.response);
      } else {
        reject(event.data.error);
      }
    });
  }

  send(request : any, transfers: any[] = []) {
    return new Promise((resolve, reject) => {
      const id = this.nextRequestId++;
      this.requestMap.set(id, [resolve, reject]);
      this.worker.postMessage({id: id, request: request}, transfers);
    });
  }
}
