package day16

type Packet interface {
	Type() uint64
	Version() uint64
	Size() uint64
}

type Operator struct {
	version, ptype, ltype uint64
	subpackets            []Packet
}

func (o *Operator) Type() uint64 {
	return o.ptype
}

func (o *Operator) Version() uint64 {
	var sum uint64

	for _, packet := range o.subpackets {
		sum += packet.Version()
	}

	return sum + o.version
}

func (o *Operator) Size() uint64 {
	var total uint64

	for _, packet := range o.subpackets {
		total += packet.Size()
	}

	// 6 Bits for the header + ltype + 1 bit for the flag
	return total + 6 + o.ltype + 1
}

type Literal struct {
	version, ptype, num, size uint64
}

func (l *Literal) Type() uint64 {
	return l.ptype
}

func (l *Literal) Version() uint64 {
	return l.version
}

func (l *Literal) Size() uint64 {
	return l.size + 6
}
