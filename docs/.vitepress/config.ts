export default {
    outDir: "../website",
    locales: {
        "/": {
            lang: 'en-US',
            title: 'Mogan.app',
            description: 'the most user-friendly distribution of GNU TeXmacs.',
        },
        "/zh/": {
            lang: 'zh-CN',
            title: '墨干编辑器',
            description: '最易用的GNU TeXmacs发行版',
        },
    },

    lastUpdated: true,
    ignoreDeadLinks: false,
    head: [],

    themeConfig: {
        locales: {
            '/': {
                selectLanguageName: 'English',
            },
            '/zh/': {
                selectLanguageName: '简体中文',
            },
        },
        nav: nav(),
        sidebar: {
            '/guide': sidebarGuide(),
            '/zh/guide': sidebarGuideZh(),
        },

        socialLinks: [
            {icon: 'github', link: 'https://github.com/XmacsLabs/mogan'}
        ],

        footer: {
            message: 'Let us enjoy exploring science and technology!',
            copyright: 'Copyright © 2022-2023 contributors of Mogan Editor'
        },
    }
}

function nav() {
    return [
        {
            text: 'Guide',
            link: '/guide/what-is-mogan',
            activeMatch: '/guide/'
        },
        {
            text: 'Forum',
            link: 'http://forum.texmacs.cn/'
        },
        {
            text: 'Language',
            items: [
                {text: '简体中文', link: '/zh/guide/what-is-mogan'},
                {text: 'English', link: '/guide/what-is-mogan'},
            ]
        },
    ]
}

function sidebarGuide() {
    return [
        {
            text: 'Introduction',
            collapsible: true,
            items: [
                {text: 'What is Mogan?', link: '/guide/what-is-mogan'},
                {text: 'The Name', link: '/guide/Name'},
            ]
        },
        {
            text: 'Getting Started',
            collapsible: true,
            items: [
                {
                    text: 'Install',
                    link: '/guide/Install'
                },
            ]
        },
        {
            text: 'Community',
            collapsible: true,
            items: [
                {
                    text: 'Contact us',
                    link: '/guide/Contact'
                },
            ]
        },
    ]
}

function sidebarGuideZh() {
    return [
        {
            text: '简介',
            collapsible: true,
            items: [
                {text: '墨干编辑器', link: '/zh/guide/what-is-mogan'},
                {text: '命名', link: '/zh/guide/Name'},
            ]
        },
        {
            text: '如何入门',
            collapsible: true,
            items: [
                {
                    text: '如何安装？',
                    link: '/zh/guide/Install'
                },
            ]
        },
        {
            text: '社区',
            collapsible: true,
            items: [
                {
                    text: '联系我们',
                    link: '/zh/guide/Contact'
                },
            ]
        },
    ]
}