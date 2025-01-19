import { Variable, GLib, bind } from "astal"
import { Astal, Gtk, Gdk } from "astal/gtk4"
import Battery from "gi://AstalBattery"

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
    >
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
                <Time />
            </box>
        </box>
    </window>
}
