// FFI for Test.Main

export const strLen = s => s.length;

export const substr = start => len => s => s.substring(start, start + len);

export const arrayLen = arr => arr.length;

export const unsafeIndex = arr => i => arr[i];
