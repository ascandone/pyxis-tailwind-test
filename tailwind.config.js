module.exports = {
  content: ["index.html", "src/**/*.elm"],
  theme: {
    extend: {
      boxShadow: {
        soft: "0 8px 42px rgba(0, 0, 0, 0.10)",
      },
    },
    fontFamily: {
      sans: ["Inter", "sans-serif"],
      serif: ["Dm serif display", "serif"],
    },
  },
}
