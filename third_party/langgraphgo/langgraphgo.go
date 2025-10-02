// File path: third_party/langgraphgo/langgraphgo.go
package langgraphgo

import "context"

type NodeFunc func(context.Context, string) (string, error)

type Graph struct {
	fn NodeFunc
}

func NewGraph(fn NodeFunc) *Graph {
	return &Graph{fn: fn}
}

func (g *Graph) Run(ctx context.Context, goal string) (string, error) {
	if g.fn == nil {
		return "", nil
	}
	return g.fn(ctx, goal)
}
