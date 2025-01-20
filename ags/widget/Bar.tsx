import { Variable, GLib, bind } from "astal"
import { Astal, Gtk, Gdk } from "astal/gtk4"
import Battery from "gi://AstalBattery"
import Wp from "gi://AstalWp"

function Audio() {
    const speaker = Wp.get_default()?.audio.defaultSpeaker!

    return <box cssClasses={["Audio", "Component", "MiddleComponent"]}>
        <image iconName={bind(speaker, "volumeIcon")} />
        <label label=
            {bind(speaker, "volume").as(v => ` ${Math.floor(v * 100)}%`)}
        />
    </box>
}

function Time({ format = "%H:%M  |  %a %e/%m" }) {
    const time = Variable<string>("").poll(1000, () =>
        GLib.DateTime.new_now_local().format(format)!)

    return <label
        cssClasses={["Time", "Component", "RightComponent"]}
        onDestroy={() => time.drop()}
        label={time()}
    />
}

function BatteryLevel() {
    const bat = Battery.get_default()

    return <box cssClasses={["Battery", "Component", "MiddleComponent"]}
        visible={bind(bat, "isPresent")}>
        <image iconName={bind(bat, "batteryIconName")} />
        <label label={bind(bat, "percentage").as(p =>
            ` ${Math.floor(p * 100)}%`
        )} />
    </box>
}

export default function Bar(monitor: Gdk.Monitor) {
    const { TOP, LEFT, RIGHT } = Astal.WindowAnchor

    return <window
        cssClasses={["Bar"]}
        gdkmonitor={monitor}
        exclusivity={Astal.Exclusivity.EXCLUSIVE}
        visible
        anchor={TOP | LEFT | RIGHT}>
        <box>
            <box hexpand halign={Gtk.Align.END} >
                <BatteryLevel />
                <Audio />
                <Time />
            </box>
        </box>
    </window>
}
