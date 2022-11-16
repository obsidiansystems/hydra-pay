let api = null;

window.setup_wallet = async () => {
    api = await cardano.LodeWallet.enable();
}

window.send_cbor = async (payload) =>
{
    if (api == null)
    {
        await window.setup_wallet();
    }

    if (api != null)
    {
        console.log("Doing the thing");
        const addrs = await api.getUsedAddresses();
        await api.signData(addrs[0], payload);
    }
}
