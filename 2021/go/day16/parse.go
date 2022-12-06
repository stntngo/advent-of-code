package day16

import (
	"encoding/hex"
	"errors"
	"io"

	"github.com/dgryski/go-bitstream"
)

type BitReader interface {
	ReadBits(int) (uint64, error)
}

const (
	HI_BIT = 0b10000
)

func Parse(r io.Reader) (Packet, error) {
	hexr := hex.NewDecoder(r)

	br := bitstream.NewReader(hexr)

	return ParsePacket(br)
}

func ParsePacket(r BitReader) (Packet, error) {
	version, err := r.ReadBits(3)
	if err != nil {
		return nil, err
	}

	ptype, err := r.ReadBits(3)
	if err != nil {
		return nil, err
	}

	switch ptype {
	case 4:
		num, size, err := ParseLiteral(r)
		if err != nil {
			return nil, err
		}

		return &Literal{
			version: version,
			ptype:   ptype,
			num:     num,
			size:    size,
		}, nil
	default:
		flag, err := r.ReadBits(1)
		if err != nil {
			return nil, err
		}

		switch flag {
		case 0:
			bitCount, err := r.ReadBits(15)
			if err != nil {
				return nil, err
			}

			packets, err := ParsePacketBits(r, bitCount)
			if err != nil {
				return nil, err
			}

			return &Operator{
				version:    version,
				ptype:      ptype,
				ltype:      15,
				subpackets: packets,
			}, nil

		case 1:
			packetCount, err := r.ReadBits(11)
			if err != nil {
				return nil, err
			}

			packets, err := ParsePacketCount(r, int(packetCount))
			if err != nil {
				return nil, err
			}

			return &Operator{
				version:    version,
				ptype:      ptype,
				ltype:      11,
				subpackets: packets,
			}, nil
		default:
			return nil, errors.New("unknown operator flag")
		}
	}
}

func ParsePacketCount(r BitReader, n int) ([]Packet, error) {
	packets := make([]Packet, n)

	for i := 0; i < n; i++ {
		packet, err := ParsePacket(r)
		if err != nil {
			return nil, err
		}

		packets[i] = packet
	}

	return packets, nil
}

func ParsePacketBits(r BitReader, n uint64) ([]Packet, error) {
	var total uint64
	var packets []Packet

	for total < n {
		packet, err := ParsePacket(r)
		if err != nil {
			return nil, err
		}

		total += packet.Size()
		packets = append(packets, packet)
	}

	return packets, nil
}

func ParseLiteral(r BitReader) (uint64, uint64, error) {
	var num, size uint64
	for {
		bits, err := r.ReadBits(5)
		if err != nil {
			return 0, 0, err
		}

		pnum := bits &^ HI_BIT
		num = (num << 4) | pnum
		size += 5

		if (bits & HI_BIT) == 0 {
			return num, size, nil
		}
	}

}
