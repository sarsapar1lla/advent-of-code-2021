
import re
from typing import List, Tuple
from itertools import groupby
from dataclasses import dataclass


@dataclass
class Beacon:
    x: int
    y: int
    z: int

    def __str__(self):
        return f"{self.x},{self.y},{self.z}"

    def as_tuple(self):
        return (self.x, self.y, self.z)

    def rotate(self, axis: str, amount: int):
        if amount == 0:
            return self
        if axis == "x":
            if amount == 1:
                return Beacon(x=self.y, y=-self.x, z=self.z)
            if amount == 2:
                return Beacon(x=-self.x, y=-self.y, z=self.z)
            if amount == 3:
                return Beacon(x=-self.y, y=self.x, z=self.z)
        if axis == "y":
            if amount == 1:
                return Beacon(x=self.x, y=self.z, z=-self.y)
            if amount == 2:
                return Beacon(x=self.x, y=-self.y, z=-self.z)
            if amount == 3:
                return Beacon(x=self.x, y=-self.z, z=self.y)
        if axis == "z":
            if amount == 1:
                return Beacon(x=-self.z, y=self.y, z=self.x)
            if amount == 2:
                return Beacon(x=-self.x, y=self.y, z=-self.z)
            if amount == 3:
                return Beacon(x=self.z, y=self.y, z=-self.x)

    def vector_to_other(self, other: 'Beacon'):
        return (self.x - other.x, self.y - other.y, self.z - other.z)


@dataclass
class Scanner:
    id: int 
    position: Tuple[int, int, int]
    beacons: List[Beacon]
    rotated_beacons: List[Beacon]

    def __str__(self):
        string = f"--- scanner {self.id} ---\n"
        for beacon in self.beacons:
            string += str(beacon) + "\n"
        return string

    def get_vectors(self):
        return [beacon.vector_to_other(other) for beacon in self.beacons for other in self.beacons if beacon != other]

    def rotate(self, axis: str, amount: int):
        self.rotated_beacons = [beacon.rotate(axis, amount) for beacon in self.beacons]


def create_scanner(lines):
    scanner_line = lines[0]
    id_search = re.search("scanner ([0-9]+)", scanner_line)
    if not id_search:
        raise ValueError("Data not in expected format.")
    scanner_id = int(id_search.group(1))
    scanner_position = (0, 0, 0) if scanner_id == 0 else None
    scanner = Scanner(id=scanner_id, position=scanner_position, beacons=[], rotated_beacons=[])

    for line in lines[1:]:
        if not line:
            break
        x, y, z = [int(c) for c in line.split(",")]
        beacon = Beacon(x, y, z)
        scanner.beacons.append(beacon)
    return scanner


def determine_position(known: Scanner, compare: Scanner, rotated=False):

    if not rotated:
        for beacon in known.beacons:
            if beacon in compare.beacons:
                shared_beacon = beacon
                break
        


def compare_scanners(known: Scanner, compare: Scanner):

    known_vectors = known.get_vectors()
    compare_vectors = known.get_vectors()

    if len(set(known_beacons + compare_beacons)) >= 12:  # at least 12 beacons in common
        determine_position(known, compare)

    for axis in "xyz":
        for amount in [1, 2, 3]:
            compare.rotate(axis, amount)
            compare_beacons = [beacon.as_tuple() for beacon in compare.rotated_beacons]
            if len(set(known_beacons + compare_beacons)) >= 12:
                determine_position(known, compare, rotated=True)
                return


def main():
    with open("test.txt") as f:
        lines = [l.replace("\n", "") for l in f.readlines()]
    blocks = [list(group) for k, group in groupby(lines, bool) if k]
    scanners = []
    for block in blocks:
        scanner = create_scanner(block)
        scanners.append(scanner)
    print(scanners[0].get_vectors())


if __name__ == "__main__":
    main()
