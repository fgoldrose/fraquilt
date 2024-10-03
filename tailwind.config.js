/** @type {import('tailwindcss').Config} */
module.exports = {
  content: [
    "./src/**/*.{html,js,ts,jsx,tsx,elm}",
  ],
  theme: {
    extend: {
      screens: {
        'tall': { 'raw': '(max-aspect-ratio: 1/2)' },
      }
    },
  },
  plugins: [],
}

