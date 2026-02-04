// FFI for Demo.Factorization.Main

export const setInnerHTML = (elementId) => (html) => () => {
  const element = document.getElementById(elementId);
  if (element) {
    element.innerHTML = html;
  } else {
    console.error(`Element with id '${elementId}' not found`);
  }
};
