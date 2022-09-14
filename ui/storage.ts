const LOCAL_STORAGE_KEY = "script";

export function saveScript(value: string) {
  localStorage.setItem(LOCAL_STORAGE_KEY, value);
}

export function deleteScript() {
  localStorage.removeItem(LOCAL_STORAGE_KEY);
}

export function getScript(): string | null {
  return localStorage.getItem(LOCAL_STORAGE_KEY);
}
