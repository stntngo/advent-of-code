package day16

import (
	"errors"
	"math"
)

func Exec(p Packet) (uint64, error) {
	switch p.Type() {
	case 0:
		o, ok := p.(*Operator)
		if !ok {
			return 0, errors.New("packet must be operator")
		}

		return sum(o)
	case 1:
		o, ok := p.(*Operator)
		if !ok {
			return 0, errors.New("packet must be operator")
		}

		return product(o)
	case 2:
		o, ok := p.(*Operator)
		if !ok {
			return 0, errors.New("packet must be operator")
		}

		return minimum(o)
	case 3:
		o, ok := p.(*Operator)
		if !ok {
			return 0, errors.New("packet must be operator")
		}

		return maximum(o)
	case 4:
		l, ok := p.(*Literal)
		if !ok {
			return 0, errors.New("packet type 4 must be literal")
		}

		return l.num, nil
	case 5:
		o, ok := p.(*Operator)
		if !ok {
			return 0, errors.New("packet must be operator")
		}

		return greaterThan(o)
	case 6:
		o, ok := p.(*Operator)
		if !ok {
			return 0, errors.New("packet must be operator")
		}

		return lessThan(o)
	case 7:
		o, ok := p.(*Operator)
		if !ok {
			return 0, errors.New("packet must be operator")
		}

		return equalTo(o)
	default:
		return 0, errors.New("unknown packet type")
	}
}

func sum(o *Operator) (uint64, error) {
	var total uint64

	for _, packet := range o.subpackets {
		value, err := Exec(packet)
		if err != nil {
			return 0, err
		}

		total += value
	}

	return total, nil
}

func product(o *Operator) (uint64, error) {
	var total uint64
	total = 1

	for _, packet := range o.subpackets {
		value, err := Exec(packet)
		if err != nil {
			return 0, err
		}

		total *= value
	}

	return total, nil
}

func minimum(o *Operator) (uint64, error) {
	min := uint64(math.MaxUint64)

	for _, packet := range o.subpackets {
		value, err := Exec(packet)
		if err != nil {
			return 0, err
		}

		if value < min {
			min = value
		}
	}

	return min, nil
}

func maximum(o *Operator) (uint64, error) {
	var max uint64

	for _, packet := range o.subpackets {
		value, err := Exec(packet)
		if err != nil {
			return 0, err
		}

		if value > max {
			max = value
		}
	}

	return max, nil
}

func greaterThan(o *Operator) (uint64, error) {
	if len(o.subpackets) != 2 {
		return 0, errors.New("greaterThan operator expects exactly 2 subpackets")
	}

	first, err := Exec(o.subpackets[0])
	if err != nil {
		return 0, err
	}

	second, err := Exec(o.subpackets[1])
	if err != nil {
		return 0, err
	}

	if first > second {
		return 1, nil
	}

	return 0, nil
}

func lessThan(o *Operator) (uint64, error) {
	if len(o.subpackets) != 2 {
		return 0, errors.New("lessThan operator expects exactly 2 subpackets")
	}

	first, err := Exec(o.subpackets[0])
	if err != nil {
		return 0, err
	}

	second, err := Exec(o.subpackets[1])
	if err != nil {
		return 0, err
	}

	if first < second {
		return 1, nil
	}

	return 0, nil
}

func equalTo(o *Operator) (uint64, error) {
	if len(o.subpackets) != 2 {
		return 0, errors.New("equalTo operator expects exactly 2 subpackets")
	}

	first, err := Exec(o.subpackets[0])
	if err != nil {
		return 0, err
	}

	second, err := Exec(o.subpackets[1])
	if err != nil {
		return 0, err
	}

	if first == second {
		return 1, nil
	}

	return 0, nil
}
