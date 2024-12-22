// https://vitepress.dev/guide/custom-theme
import { h } from 'vue'
import Theme from 'vitepress/theme'
import './style.css'
import MyLayout from "./MyLayout.vue"

import DownloadButtonWindows from '../components/ButtonZh/DownloadButtonWindows.vue'
import DownloadButtonMacOS from '../components/ButtonZh/DownloadButtonMacOS.vue'
import DownloadButtonUbuntu from '../components/ButtonZh/DownloadButtonUbuntu.vue'

import EnDownloadButtonWindows from '../components/ButtonEn/EnDownloadButtonWindows.vue'
import EnDownloadButtonMacOS from '../components/ButtonEn/EnDownloadButtonMacOS.vue'
import EnDownloadButtonUbuntu from '../components/ButtonEn/EnDownloadButtonUbuntu.vue'

import SHA256Button from '../components/ButtonZh/SHA256Button.vue'
import EnSHA256Button from '../components/ButtonEn/EnSHA256Button.vue'

export default {
  ...Theme,
  Layout: MyLayout,
  async enhanceApp({ app, router, siteData }) {
    app.component('DownloadButtonWindows', DownloadButtonWindows)
    app.component('DownloadButtonMacOS', DownloadButtonMacOS)
    app.component('DownloadButtonUbuntu', DownloadButtonUbuntu)
    app.component('EnDownloadButtonWindows', EnDownloadButtonWindows)
    app.component('EnDownloadButtonMacOS', EnDownloadButtonMacOS)
    app.component('EnDownloadButtonUbuntu', EnDownloadButtonUbuntu)
    app.component('SHA256Button',SHA256Button)
    app.component('EnSHA256Button',EnSHA256Button)
  }
}
