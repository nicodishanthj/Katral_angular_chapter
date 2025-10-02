// File path: internal/graph/memory/service.go
package memory

import (
	"context"
	"fmt"
	"sort"
	"strings"
	"sync"
	"time"

	"github.com/nicodishanthj/Katral_phase1/internal/graph"
	"github.com/nicodishanthj/Katral_phase1/internal/kb"
)

// Service provides an in-memory implementation of graph.DependencyService by
// synthesizing relationships from the knowledge base documents.
type Service struct {
	mu        sync.RWMutex
	programs  map[string]*programInfo
	outgoing  map[string]map[string]struct{}
	incoming  map[string]map[string]struct{}
	assetsIn  map[string]map[string]struct{}
	assetsOut map[string]map[string]struct{}

	cacheMu      sync.RWMutex
	cacheTTL     time.Duration
	depCache     map[string]cacheEntry
	impactCache  map[string]cacheEntry
	relatedCache map[string]cacheEntry
}

type cacheEntry struct {
	neighbors []graph.Neighbor
	expires   time.Time
}

type Option func(*Service)

func WithCacheTTL(ttl time.Duration) Option {
	return func(s *Service) {
		s.cacheTTL = ttl
	}
}

type programInfo struct {
	ID      string
	Name    string
	Source  string
	Inputs  []string
	Outputs []string
}

// NewService constructs an empty dependency service. Call Refresh with the
// current document set before issuing queries.
func NewService(opts ...Option) *Service {
	svc := &Service{
		programs:     make(map[string]*programInfo),
		outgoing:     make(map[string]map[string]struct{}),
		incoming:     make(map[string]map[string]struct{}),
		assetsIn:     make(map[string]map[string]struct{}),
		assetsOut:    make(map[string]map[string]struct{}),
		cacheTTL:     time.Minute,
		depCache:     make(map[string]cacheEntry),
		impactCache:  make(map[string]cacheEntry),
		relatedCache: make(map[string]cacheEntry),
	}
	for _, opt := range opts {
		if opt != nil {
			opt(svc)
		}
	}
	if svc.cacheTTL <= 0 {
		svc.cacheTTL = time.Minute
	}
	return svc
}

// Refresh rebuilds the in-memory graph using the provided documents.
func (s *Service) Refresh(docs []kb.Doc) {
	s.mu.Lock()
	defer s.mu.Unlock()

	s.programs = make(map[string]*programInfo)
	s.outgoing = make(map[string]map[string]struct{})
	s.incoming = make(map[string]map[string]struct{})
	s.assetsIn = make(map[string]map[string]struct{})
	s.assetsOut = make(map[string]map[string]struct{})

	s.cacheMu.Lock()
	s.depCache = make(map[string]cacheEntry)
	s.impactCache = make(map[string]cacheEntry)
	s.relatedCache = make(map[string]cacheEntry)
	s.cacheMu.Unlock()

	for _, doc := range docs {
		programID := normalizeProgram(doc.Program)
		if programID == "" {
			continue
		}
		switch strings.ToLower(strings.TrimSpace(doc.Type)) {
		case "metadata":
			info := ensureProgram(s.programs, programID, doc)
			for _, call := range doc.Calls {
				calleeID := normalizeProgram(call)
				if calleeID == "" || calleeID == programID {
					continue
				}
				addEdge(s.outgoing, programID, calleeID)
				addEdge(s.incoming, calleeID, programID)
			}
			if len(doc.Inputs) > 0 {
				info.Inputs = dedupeStrings(doc.Inputs)
				s.assetsIn[programID] = toSet(info.Inputs)
			}
			if len(doc.Outputs) > 0 {
				info.Outputs = dedupeStrings(doc.Outputs)
				s.assetsOut[programID] = toSet(info.Outputs)
			}
		case "flow", "cics_flow", "mq_flow":
			ensureProgram(s.programs, programID, doc)
		default:
			ensureProgram(s.programs, programID, doc)
		}
	}
}

// Dependencies returns downstream programs reached via CALL edges up to the
// provided depth.
func (s *Service) Dependencies(ctx context.Context, program string, depth int) ([]graph.Neighbor, error) {
	return s.traverse(ctx, program, depth, graph.NeighborKindDependency, s.outgoing)
}

// Impacts returns upstream callers for the specified program.
func (s *Service) Impacts(ctx context.Context, program string, depth int) ([]graph.Neighbor, error) {
	return s.traverse(ctx, program, depth, graph.NeighborKindImpact, s.incoming)
}

// Related returns programs that either call/be called by the target or share IO
// assets.
func (s *Service) Related(ctx context.Context, program string, limit int) ([]graph.Neighbor, error) {
	programID := normalizeProgram(program)
	if programID == "" {
		return nil, nil
	}
	if limit <= 0 {
		limit = 10
	}
	cacheKey := fmt.Sprintf("%s|%d", programID, limit)
	if neighbors, ok := s.getCachedNeighbors(graph.NeighborKindRelated, cacheKey); ok {
		return neighbors, nil
	}

	s.mu.RLock()
	defer s.mu.RUnlock()

	if _, ok := s.programs[programID]; !ok {
		return nil, nil
	}
	scores := make(map[string]*neighborScore)
	for neighbor := range s.outgoing[programID] {
		scores[neighbor] = &neighborScore{Weight: 1}
	}
	for neighbor := range s.incoming[programID] {
		score := scores[neighbor]
		if score == nil {
			score = &neighborScore{}
			scores[neighbor] = score
		}
		score.Weight += 1
	}
	for peerID, peerInfo := range s.programs {
		if peerID == programID {
			continue
		}
		shared := sharedAssets(s.assetsIn[programID], s.assetsOut[programID], s.assetsIn[peerID], s.assetsOut[peerID])
		if shared == 0 {
			continue
		}
		score := scores[peerID]
		if score == nil {
			score = &neighborScore{}
			scores[peerID] = score
		}
		score.Weight += float64(shared)
		if len(score.Chain) == 0 {
			score.Chain = []string{programID, peerID}
		}
		if peerInfo != nil && score.Name == "" {
			score.Name = peerInfo.Name
			score.Source = peerInfo.Source
		}
	}
	if len(scores) == 0 {
		return nil, nil
	}
	neighbors := make([]graph.Neighbor, 0, len(scores))
	for id, score := range scores {
		if _, exists := s.programs[id]; !exists {
			continue
		}
		info := s.programs[id]
		chain := score.Chain
		if len(chain) == 0 {
			chain = []string{programID, id}
		}
		neighbors = append(neighbors, graph.Neighbor{
			Program:  id,
			Name:     info.Name,
			Source:   info.Source,
			Distance: 1,
			Weight:   score.Weight,
			Chain:    append([]string(nil), chain...),
			Kind:     graph.NeighborKindRelated,
		})
	}
	sort.SliceStable(neighbors, func(i, j int) bool {
		if neighbors[i].Weight == neighbors[j].Weight {
			return neighbors[i].Program < neighbors[j].Program
		}
		return neighbors[i].Weight > neighbors[j].Weight
	})
	if limit > 0 && len(neighbors) > limit {
		neighbors = neighbors[:limit]
	}
	if len(neighbors) > 0 {
		s.storeCachedNeighbors(graph.NeighborKindRelated, cacheKey, neighbors)
	}
	return neighbors, nil
}

type neighborScore struct {
	Weight float64
	Chain  []string
	Name   string
	Source string
}

func (s *Service) traverse(ctx context.Context, program string, depth int, kind graph.NeighborKind, edges map[string]map[string]struct{}) ([]graph.Neighbor, error) {
	if depth <= 0 {
		depth = 3
	}
	programID := normalizeProgram(program)
	if programID == "" {
		return nil, nil
	}
	cacheKey := fmt.Sprintf("%s|%d", programID, depth)
	if neighbors, ok := s.getCachedNeighbors(kind, cacheKey); ok {
		return neighbors, nil
	}

	s.mu.RLock()
	defer s.mu.RUnlock()

	if _, ok := s.programs[programID]; !ok {
		return nil, nil
	}

	type path struct {
		id    string
		depth int
		chain []string
	}

	queue := []path{{id: programID, depth: 0, chain: []string{programID}}}
	visited := map[string]struct{}{programID: {}}
	var results []graph.Neighbor

	for len(queue) > 0 {
		select {
		case <-ctx.Done():
			return results, ctx.Err()
		default:
		}
		current := queue[0]
		queue = queue[1:]
		if current.depth >= depth {
			continue
		}
		children := edges[current.id]
		for child := range children {
			if _, seen := visited[child]; seen {
				continue
			}
			visited[child] = struct{}{}
			chain := append(append([]string(nil), current.chain...), child)
			info := s.programs[child]
			neighbor := graph.Neighbor{
				Program:  child,
				Distance: current.depth + 1,
				Chain:    chain,
				Kind:     kind,
				Weight:   1,
			}
			if info != nil {
				neighbor.Name = info.Name
				neighbor.Source = info.Source
			}
			results = append(results, neighbor)
			queue = append(queue, path{id: child, depth: current.depth + 1, chain: chain})
		}
	}
	sort.SliceStable(results, func(i, j int) bool {
		if results[i].Distance == results[j].Distance {
			return results[i].Program < results[j].Program
		}
		return results[i].Distance < results[j].Distance
	})
	if len(results) > 0 {
		s.storeCachedNeighbors(kind, cacheKey, results)
	}
	return results, nil
}

func (s *Service) getCachedNeighbors(kind graph.NeighborKind, key string) ([]graph.Neighbor, bool) {
	if s.cacheTTL <= 0 || key == "" {
		return nil, false
	}
	var cache map[string]cacheEntry
	s.cacheMu.RLock()
	switch kind {
	case graph.NeighborKindDependency:
		cache = s.depCache
	case graph.NeighborKindImpact:
		cache = s.impactCache
	default:
		cache = s.relatedCache
	}
	entry, ok := cache[key]
	s.cacheMu.RUnlock()
	if !ok {
		return nil, false
	}
	if time.Now().After(entry.expires) {
		s.cacheMu.Lock()
		switch kind {
		case graph.NeighborKindDependency:
			delete(s.depCache, key)
		case graph.NeighborKindImpact:
			delete(s.impactCache, key)
		default:
			delete(s.relatedCache, key)
		}
		s.cacheMu.Unlock()
		return nil, false
	}
	neighbors := append([]graph.Neighbor(nil), entry.neighbors...)
	return neighbors, true
}

func (s *Service) storeCachedNeighbors(kind graph.NeighborKind, key string, neighbors []graph.Neighbor) {
	if s.cacheTTL <= 0 || key == "" || len(neighbors) == 0 {
		return
	}
	entry := cacheEntry{neighbors: append([]graph.Neighbor(nil), neighbors...), expires: time.Now().Add(s.cacheTTL)}
	s.cacheMu.Lock()
	switch kind {
	case graph.NeighborKindDependency:
		s.depCache[key] = entry
	case graph.NeighborKindImpact:
		s.impactCache[key] = entry
	default:
		s.relatedCache[key] = entry
	}
	s.cacheMu.Unlock()
}

func ensureProgram(programs map[string]*programInfo, id string, doc kb.Doc) *programInfo {
	info, ok := programs[id]
	if !ok {
		info = &programInfo{ID: id}
		programs[id] = info
	}
	if info.Name == "" {
		info.Name = strings.TrimSpace(doc.Program)
	}
	if info.Source == "" {
		info.Source = strings.TrimSpace(doc.SourcePath)
	}
	return info
}

func addEdge(edges map[string]map[string]struct{}, from, to string) {
	neighbors := edges[from]
	if neighbors == nil {
		neighbors = make(map[string]struct{})
		edges[from] = neighbors
	}
	neighbors[to] = struct{}{}
}

func normalizeProgram(program string) string {
	return strings.ToUpper(strings.TrimSpace(program))
}

func dedupeStrings(values []string) []string {
	seen := make(map[string]struct{}, len(values))
	out := make([]string, 0, len(values))
	for _, v := range values {
		key := strings.TrimSpace(v)
		if key == "" {
			continue
		}
		keyUpper := strings.ToUpper(key)
		if _, exists := seen[keyUpper]; exists {
			continue
		}
		seen[keyUpper] = struct{}{}
		out = append(out, key)
	}
	sort.Strings(out)
	return out
}

func toSet(values []string) map[string]struct{} {
	if len(values) == 0 {
		return nil
	}
	set := make(map[string]struct{}, len(values))
	for _, v := range values {
		trimmed := strings.TrimSpace(v)
		if trimmed == "" {
			continue
		}
		set[strings.ToUpper(trimmed)] = struct{}{}
	}
	return set
}

func sharedAssets(inA, outA, inB, outB map[string]struct{}) int {
	count := 0
	count += intersectCount(inA, inB)
	count += intersectCount(outA, outB)
	count += intersectCount(outA, inB)
	count += intersectCount(inA, outB)
	return count
}

func intersectCount(left, right map[string]struct{}) int {
	if len(left) == 0 || len(right) == 0 {
		return 0
	}
	var smaller, larger map[string]struct{}
	if len(left) < len(right) {
		smaller, larger = left, right
	} else {
		smaller, larger = right, left
	}
	count := 0
	for key := range smaller {
		if _, ok := larger[key]; ok {
			count++
		}
	}
	return count
}
