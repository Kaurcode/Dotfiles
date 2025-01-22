import { Variable, GLib, bind } from "astal"
import { Astal, Gtk, Gdk } from "astal/gtk4"
import AstalHyprland from "gi://AstalHyprland"
import Battery from "gi://AstalBattery"
import Wp from "gi://AstalWp"

function getWorkspaceClassesByID({
    workspaceID, hyprland, focusedWorkspace, clients
}: {
    workspaceID: number,
    hyprland: AstalHyprland.Hyprland,
    focusedWorkspace: Binding<AstalHyprland.Workspace>,
    clients: Binding<AstalHyprland.Client>
}): Variable<string[]> {
    return Variable.derive(
        [focusedWorkspace, clients],
        (focusedWorkspace, _) => {
            const classes = ["workspace-indicator"];

            const isActive = focusedWorkspace.id === workspaceID;
            isActive && classes.push("active-workspace")

            const isOccupied = hyprland.get_workspace(workspaceID)?.get_clients()
                .length > 0;

            isOccupied && classes.push("occupied-workspace")

            return classes;
        }
    );
}




function Workspace({ workspace, assignedClasses }: {
    workspace: AstalHyprland.Workspace,
    assignedClasses: Variable<string[]>
}): JSX.Element {
    return (
        <button
            cssClasses={assignedClasses()}
            onDestroy={() => assignedClasses.drop()}
            onClicked={() => workspace.focus()}
        />
    );

}

function WorkspacePanel(): JSX.Element {
    const hyprland: AstalHyprland.Hyprland = AstalHyprland.get_default()

    const focusedWorkspace: Binding<AstalHyprland.Workspace> =
        bind(hyprland, "focusedWorkspace");

    const clients: Binding<AstalHyprland.Client[]> = bind(hyprland, "clients");

    return <box cssClasses={["workspace-container"]}>
        {Array.from({ length: 10 }, (_, i) => i + 1).map((workspaceID) => {
            const assignedClasses: Variable<string[]> = getWorkspaceClassesByID(
                { workspaceID, hyprland, focusedWorkspace, clients }
            );

            return (
                <Workspace
                    key={workspaceID}
                    workspace={
                        AstalHyprland.Workspace.dummy(workspaceID, null)
                    }
                    assignedClasses={assignedClasses}
                />
            );
        })}
    </box>
};

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
            {/*
            <box hexpand halign={Gtk.Align.START} >
                <WorkspacePanel />
            </box>
            */}
            <box hexpand halign={Gtk.Align.END} >
                <BatteryLevel />
                <Audio />
                <Time />
            </box>
        </box>
    </window>
}
