// File path: internal/retriever/cache.go
package retriever

import (
	"container/list"
	"sync"
)

type cacheEntry struct {
	key   string
	value interface{}
}

type graphCache struct {
	mu       sync.Mutex
	capacity int
	items    map[string]*list.Element
	ll       *list.List
}

func newGraphCache(size int) *graphCache {
	if size <= 0 {
		size = 64
	}
	return &graphCache{
		capacity: size,
		items:    make(map[string]*list.Element, size),
		ll:       list.New(),
	}
}

func (c *graphCache) Get(key string) (interface{}, bool) {
	c.mu.Lock()
	defer c.mu.Unlock()
	if c == nil || c.ll == nil {
		return nil, false
	}
	if elem, ok := c.items[key]; ok {
		c.ll.MoveToFront(elem)
		if entry, ok := elem.Value.(cacheEntry); ok {
			return entry.value, true
		}
	}
	return nil, false
}

func (c *graphCache) Set(key string, value interface{}) {
	c.mu.Lock()
	defer c.mu.Unlock()
	if c == nil || c.ll == nil {
		return
	}
	if elem, ok := c.items[key]; ok {
		elem.Value = cacheEntry{key: key, value: value}
		c.ll.MoveToFront(elem)
		return
	}
	elem := c.ll.PushFront(cacheEntry{key: key, value: value})
	c.items[key] = elem
	if c.ll.Len() > c.capacity {
		tail := c.ll.Back()
		if tail != nil {
			c.ll.Remove(tail)
			if entry, ok := tail.Value.(cacheEntry); ok {
				delete(c.items, entry.key)
			}
		}
	}
}

func (c *graphCache) Purge() {
	c.mu.Lock()
	defer c.mu.Unlock()
	if c == nil {
		return
	}
	c.items = make(map[string]*list.Element)
	c.ll = list.New()
}
