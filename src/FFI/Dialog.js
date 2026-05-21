export const openModal = (selector) => () => {
    document.querySelector(selector).showModal();
};

export const close = (selector) => () => {
    document.querySelector(selector).close();
};