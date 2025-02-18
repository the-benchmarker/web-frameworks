module.exports = {
  apps: [
    {
      script: "dist/main.js",
      instances: "max",
      exec_mode: "cluster",
    },
  ],
};
