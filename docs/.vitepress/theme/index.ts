// https://vitepress.dev/guide/custom-theme
import { h } from 'vue'
import Theme from 'vitepress/theme'
import './style.css'
import MyLayout from "./MyLayout.vue"

import DownloadButtonWindows from '../components/Button/DownloadButtonWindows.vue'
import DownloadButtonMacOS from '../components/Button/DownloadButtonMacOS.vue'
import DownloadButtonUbuntu from '../components/Button/DownloadButtonUbuntu.vue'
import SHA256Button from '../components/Button/SHA256Button.vue'

export default {
  ...Theme,
  Layout: MyLayout,
  async enhanceApp({ app, router, siteData }) {
    app.component('DownloadButtonWindows', DownloadButtonWindows)
    app.component('DownloadButtonMacOS', DownloadButtonMacOS)
    app.component('DownloadButtonUbuntu', DownloadButtonUbuntu)
    app.component('SHA256Button',SHA256Button)
  }
}
