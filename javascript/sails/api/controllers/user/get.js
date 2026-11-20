module.exports = {
  fn: async function () {
    this.res.send(this.req.params.id);
  },
};
