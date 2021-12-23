
import re
from dataclasses import dataclass, field
from typing import List


@dataclass
class Region:
    x_min: int
    x_max: int
    y_min: int
    y_max: int
    z_min: int
    z_max: int
    holes: List['Region'] = field(default_factory=list)
    on: bool = False

    def count_on(self):
        if self.on:
            volume = (self.x_max + 1 - self.x_min) * (self.y_max + 1 - self.y_min) * (self.z_max + 1 - self.z_min)
            for region in self.holes:
                if not region.on:
                    volume -= region.count_on()
            return volume
        volume = 0
        for region in self.holes:
            if region.on:
                volume += region.count_on()
        return volume
        

    def is_within_region(self, other: 'Region'):
        within = (
            other.x_min <= self.x_min <= self.x_max <= other.x_max
        and other.y_min <= self.y_min <= self.y_max <= other.y_max
        and other.z_min <= self.z_min <= self.z_max <= other.z_max
        )
        if within:
            return True
        return False

    def get_common_region(self, other: 'Region'):
        if self.x_max < other.x_min or self.x_min > other.x_max:
            return None
        if self.y_max < other.y_min or self.y_min > other.y_max:
            return None
        if self.z_max < other.z_min or self.z_min > other.z_max:
            return None

        x_min = max(self.x_min, other.x_min)
        x_max = min(self.x_max, other.x_max)
        y_min = max(self.y_min, other.y_min)
        y_max = min(self.y_max, other.y_max)
        z_min = max(self.z_min, other.z_min)
        z_max = min(self.z_max, other.z_max)
        return Region(
            x_min=x_min,
            x_max=x_max,
            y_min=y_min,
            y_max=y_max,
            z_min=z_min,
            z_max=z_max,
            on=other.on
        )


@dataclass
class Command:
    action: str
    region: Region


def get_commands(lines):
    command_pattern = r"(on|off) x=(-?[0-9]+)..(-?[0-9]+),y=(-?[0-9]+)..(-?[0-9]+),z=(-?[0-9]+)..(-?[0-9]+)"
    commands = []
    for line in lines:
        command_search = re.search(command_pattern, line)
        if not command_search:
            raise ValueError(f"Command {line} is not a valid command.")
        action = command_search.group(1)
        x_min = int(command_search.group(2))
        x_max = int(command_search.group(3))
        y_min = int(command_search.group(4))
        y_max = int(command_search.group(5))
        z_min = int(command_search.group(6))
        z_max = int(command_search.group(7))
        region = Region(
            x_min=x_min,
            x_max=x_max,
            y_min=y_min,
            y_max=y_max,
            z_min=z_min,
            z_max=z_max,
            on = True if action == "on" else False
        )
        command = Command(
            action=action,
            region=region
        )
        commands.append(command)
    return commands


def execute_commands(commands, initial_region: Region):
    regions = [initial_region]
    for command in commands:
        new_regions = []
        for region in regions:
            if command.region.is_within_region(region) and command.region.on == region.on:
                continue  # skip steps which won't change the status of any cubes
            if any(command.region.is_within_region(hole) for hole in region.holes):
                continue
            new_region = region.get_common_region(command.region)
            if not new_region:
                continue
            region.holes.append(new_region)
            new_regions.append(new_region)
        regions.extend(new_regions)
    return regions


def main():
    with open("test.txt") as f:
        lines = [l.replace("\n", "") for l in f.readlines()]
    commands = get_commands(lines)
    maximum_area = 50
    initial_region = Region(
        x_min=-maximum_area,
        x_max=maximum_area,
        y_min=-maximum_area,
        y_max=maximum_area,
        z_min=-maximum_area,
        z_max=maximum_area,
        on=False
    )
    regions = execute_commands(commands, initial_region)
    print(initial_region.count_on())


if __name__ == "__main__":
    main()
