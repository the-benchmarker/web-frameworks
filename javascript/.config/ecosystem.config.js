module.exports = {
  apps: [
    {
      name: "app",
      script: "/usr/src/app/app.js",
      instances: "max",
      exec_mode: "cluster"
    }
  ]
};
