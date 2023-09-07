//https://nitro.unjs.io/config
export default defineNitroConfig({
    // File with [] in name can't be copied properly
    handlers: [
        {
            route: '/user/:id',
            handler: './user-id.get.ts'
        }
    ]
});
