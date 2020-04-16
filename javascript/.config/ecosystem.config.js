module.exports = {
  apps: [
    {
      name: "app",
      script: "/opt/web/app.js",
      instances: "max",
      exec_mode: "cluster"
    }
  ]
};
