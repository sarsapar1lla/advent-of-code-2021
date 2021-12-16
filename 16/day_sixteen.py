
from typing import List
from dataclasses import dataclass, field
from functools import reduce


@dataclass
class Packet:
    version: int
    type_id: int
    value: int = None
    length_type: int = None
    sub_packet_total_length: int = None
    total_sub_packets: int = None
    sub_packets: List['Packet'] = field(default_factory=list)


def decode_string(string):
    hex_to_bytes = {
        "0": "0000",
        "1": "0001",
        "2": "0010",
        "3": "0011",
        "4": "0100",
        "5": "0101",
        "6": "0110",
        "7": "0111",
        "8": "1000",
        "9": "1001",
        "A": "1010",
        "B": "1011",
        "C": "1100",
        "D": "1101",
        "E": "1110",
        "F": "1111",
    }

    decoded = ""
    for c in string:
        decoded += hex_to_bytes[c]
    return decoded


def get_packets_from_bytes(bytes):
    version = int(bytes[:3], 2)
    type_id = int(bytes[3:6], 2)
    substring = bytes[6:]
    if type_id == 4:  # literal value
        value_bytes = ""
        while len(substring) >= 5:
            byte_group = substring[:5]
            value_bytes += byte_group[1:]
            substring = substring[5:]
            if byte_group[0] == "0":  # last group in packet
                break
        value = int(value_bytes, 2)
        return Packet(version=version, type_id=type_id, value=value), substring

    # handle operators
    length_type = int(substring[0])
    substring = substring[1:]
    sub_packets = []
    if length_type == 0:
        sub_packet_total_length = int(substring[:15], 2)
        substring = substring[15:]
        processed_bytes = 0
        current_substring_length = len(substring)
        while processed_bytes < sub_packet_total_length and len(substring) >= 11:
            sub_packet, substring = get_packets_from_bytes(substring)
            sub_packets.append(sub_packet)
            processed_bytes = current_substring_length - len(substring)
            print(f"Processed {processed_bytes} of {sub_packet_total_length}.")
        return Packet(
            version=version, 
            type_id=type_id, 
            length_type=length_type,
            sub_packet_total_length=sub_packet_total_length,
            sub_packets=sub_packets
        ), substring
    if length_type == 1:
        total_packets = int(substring[:11], 2)
        substring = substring[11:]
        while len(sub_packets) < total_packets:
            print(f"Processed {len(sub_packets)} of {total_packets}.")
            sub_packet, substring = get_packets_from_bytes(substring)
            sub_packets.append(sub_packet)
        return Packet(
            version=version, 
            type_id=type_id, 
            length_type=length_type,
            total_sub_packets=total_packets,
            sub_packets=sub_packets
        ), substring


def unpack(packets):
    unpacked = []
    for packet in packets:
        unpacked.append(packet)
        if packet.sub_packets:
            unpacked.extend(unpack(packet.sub_packets))
    return unpacked


def sum_packet_versions(packets):
    total = 0
    for packet in packets:
        total += packet.version
    return total


def calculate_packet_values(packets: List[Packet]):

    for packet in packets:
        type_id = packet.type_id
        calculate_packet_values(packet.sub_packets)
        values = [s.value for s in packet.sub_packets]
        if packet.value is not None:
            continue

        if type_id == 0:
            packet.value = sum(values)
        elif type_id == 1:
            if len(values) == 1:
                packet.value = values[0]
                continue
            packet.value = reduce(lambda x, y: x * y, values)
        elif type_id == 2:
            packet.value = min(values)
        elif type_id == 3:
            packet.value = max(values)
        elif type_id == 4:
            continue  # value already populated
        elif type_id == 5:
            packet.value = 1 if values[0] > values[1] else 0
        elif type_id == 6:
            packet.value = 1 if values[0] < values[1] else 0
        elif type_id == 7:
            packet.value = 1 if values[0] == values[1] else 0
        else:
            raise ValueError("Invalid type_id")
    return packets


def main():
    with open("data.txt") as f:
        string = f.readline().replace("\n", "")
    test = ""
    if test:
        string = test
    decoded = decode_string(string)
    packet, remainder = get_packets_from_bytes(decoded)
    packets = unpack([packet])
    print(sum_packet_versions(packets))
    calculate_packet_values([packet])
    print(packet.value)


if __name__ == "__main__":
    main()
