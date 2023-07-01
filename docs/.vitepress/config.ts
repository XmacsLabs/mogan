export default {
    title: 'Mogan Editor',
    outDir: "../website",
    locales: {
        "/": {
            lang: 'en-US',
            title: 'Mogan Editor',
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
        siteTitle: 'Mogan Editor',
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
                {text: 'Major Features', link: '/guide/Feature'},
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
        // {
        //     text: 'ChangeLog',
        //     collapsible: false,
        //     items: [
        //         {
        //             text: 'v1.1.3',
        //             link: '/guide/ChangeLog_v1.1.3.md'
        //         },
        //     ]
        // },
    ]
}

function sidebarGuideZh() {
    return [
        {
            text: '简介',
            collapsible: true,
            items: [
                {text: '墨干编辑器', link: '/zh/guide/what-is-mogan'},
                {text: '主要功能', link: '/guide/Feature'},
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
        {
            text: '变更记录',
            collapsible: true,
            items: [
                {
                    text: 'v1.1.3 (2023/06/05)',
                    link: '/zh/guide/ChangeLog_v1.1.3.md'
                },
                {
                    text: 'v1.1.2 (2023/04/09)',
                    link: '/zh/guide/ChangeLog_v1.1.2.md'
                },
            ]
        },
    ]
}