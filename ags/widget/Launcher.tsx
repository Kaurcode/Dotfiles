import AstalApps from "gi://AstalApps"
import { Variable } from "astal"
import { Astal, App, Gtk, Gdk } from "astal/gtk4"

export const WINDOW_NAME = "launcher"

function hide() {
    App.get_window(WINDOW_NAME)?.hide()
}

function SearchEntry({ apps, text }: { apps: AstalApps.Apps, text: Variable<string> }): JSX.Element {
    const onEnter = () => {
        hide();
        apps.fuzzy_query(text.get())?.[0].launch();
    }

    return (
        <entry placeholderText="Search"
            text={text()}
            onChanged={self => text.set(self.text)}
            onActivate={onEnter} />
    );
}

function AppEntry({ app }: { app: AstalApps.Application }): JSX.Element {
    return (
        <button
            onClicked={() => {
                hide();
                app.launch();
            }}
        >
            <box>
                <image iconName={app.iconName} />
                <box vertical>
                    <label label={app.name} />
                    {app.description && (
                        <label label={app.description} />
                    )}
                </box>
            </box>
        </button>
    );
}

function AppMenu(
    { apps, text }: { apps: AstalApps.Apps, text: Variable<string> }
): JSX.Element {
    const appList = text((text) => apps.fuzzy_query(text));

    return (
        <Gtk.ScrolledWindow>
            <box vertical>
                {appList.as(
                    (appList) => appList.map((app) => <AppEntry app={app} />)
                )}
            </box>
        </Gtk.ScrolledWindow>
    );
}

export default function Applauncher() {
    const apps = new AstalApps.Apps();
    const text = Variable("")

    return (
        <window
            name={WINDOW_NAME}
            visible
            keymode={Astal.Keymode.EXCLUSIVE}
            onShow={() => text.set("")}
            onKeyPressed={(self, keyval) => {
                if (keyval === Gdk.KEY_Escape) {
                    self.hide()
                }
            }}
        >
            <box vertical vexpand={false}>
                <SearchEntry apps={apps} text={text} />
                <AppMenu apps={apps} text={text} />
            </box>
        </window>
    );
}
