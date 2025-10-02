// File path: internal/context/cache.go
package context

import (
	"sync"
	"time"
)

type cacheEntry struct {
	value     interface{}
	expiresAt time.Time
}

type ttlCache struct {
	mu   sync.RWMutex
	data map[string]cacheEntry
	ttl  time.Duration
}

func newTTLCache(ttl time.Duration) *ttlCache {
	if ttl <= 0 {
		ttl = time.Minute
	}
	return &ttlCache{data: make(map[string]cacheEntry), ttl: ttl}
}

func (c *ttlCache) get(key string) (interface{}, bool) {
	if c == nil {
		return nil, false
	}
	c.mu.RLock()
	entry, ok := c.data[key]
	c.mu.RUnlock()
	if !ok {
		return nil, false
	}
	if time.Now().After(entry.expiresAt) {
		c.mu.Lock()
		delete(c.data, key)
		c.mu.Unlock()
		return nil, false
	}
	return entry.value, true
}

func (c *ttlCache) set(key string, value interface{}) {
	if c == nil {
		return
	}
	c.mu.Lock()
	c.data[key] = cacheEntry{value: value, expiresAt: time.Now().Add(c.ttl)}
	c.mu.Unlock()
}
