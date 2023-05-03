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
            message: 'This website is released under the MIT License.',
            copyright: 'Copyright © 2022 XmacsLabs contributors'
        },
    }
}

function nav() {
    return [
        {text: 'Guide', link: '/guide/what-is-mogan', activeMatch: '/guide/'},
        {
            text: 'Language',
            items: [
                {text: '简体中文', link: '/zh/guide/what-is-mogan'},
                {text: 'English', link: '/guide/what-is-mogan'},
            ]
        },
        {
            text: 'Github Issues',
            link: 'https://github.com/XmacsLabs/mogan/issues'
        },
        {
            text: 'Community',
            link: 'https://github.com/XmacsLabs/mogan/discussions'
        }
    ]
}

function navZh() {
    return [
        {text: 'Guide', link: '/zh/guide/what-is-mogan', activeMatch: '/zh/guide/'},
        {
            text: 'Gitee Issues',
            link: 'https://gitee.com/XmacsLabs/mogan/issues'
        },
        {
            text: 'Community',
            link: 'https://github.com/XmacsLabs/mogan/discussions'
        }
    ]
}

function sidebarGuide() {
    return [
        {
            text: 'Introduction',
            collapsible: true,
            items: [
                {text: 'What is Mogan?', link: '/guide/what-is-mogan'},
            ]
        },
        {
            text: 'Install',
            collapsible: true,
            items: [
                {
                    text: 'Install',
                    link: '/guide/Install'
                },
            ]
        },
        {
            text: 'Development',
            collapsible: true,
            items: [
                {text: 'Contributing', link: '/guide/CONTRIBUTING'},
                {text: 'Linux', link: '/guide/Develop_on_Linux'},
                {text: 'macOS', link: '/guide/Develop_on_macOS'},
                {text: 'Windows', link: '/guide/Develop_on_Windows'},
            ]
        }
    ]
}

function sidebarGuideZh() {
    return [
        {
            text: '简介',
            collapsible: true,
            items: [
                {text: '墨干编辑器', link: '/zh/guide/what-is-mogan'},
            ]
        },
        {
            text: '安装',
            collapsible: true,
            items: [
                {
                    text: '如何安装？',
                    link: '/zh/guide/Install'
                },
            ]
        },
        {
            text: '参与开发',
            collapsible: true,
            items: [
                {text: '如何参与？', link: '/zh/guide/CONTRIBUTING'},
                {text: 'Linux', link: '/zh/guide/Develop_on_Linux'},
                {text: 'macOS', link: '/zh/guide/Develop_on_macOS'},
                {text: 'Windows', link: '/zh/guide/Develop_on_Windows'},
            ]
        }
    ]
}