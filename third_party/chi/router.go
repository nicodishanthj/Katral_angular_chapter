// File path: third_party/chi/router.go
package chi

import (
	"net/http"
	"strings"
)

type Middleware func(http.Handler) http.Handler

type Router interface {
	http.Handler
	Use(middlewares ...Middleware)
	Method(method, pattern string, handler http.Handler)
	Handle(pattern string, handler http.Handler)
	Get(pattern string, handlerFn http.HandlerFunc)
	Post(pattern string, handlerFn http.HandlerFunc)
}

type Mux struct {
	routes      []route
	middlewares []Middleware
}

type route struct {
	method  string
	pattern string
	handler http.Handler
}

func NewRouter() *Mux {
	return &Mux{}
}

func (m *Mux) Use(middlewares ...Middleware) {
	m.middlewares = append(m.middlewares, middlewares...)
}

func (m *Mux) Method(method, pattern string, handler http.Handler) {
	m.routes = append(m.routes, route{method: strings.ToUpper(method), pattern: pattern, handler: handler})
}

func (m *Mux) Handle(pattern string, handler http.Handler) {
	m.Method("GET", pattern, handler)
}

func (m *Mux) Get(pattern string, handlerFn http.HandlerFunc) {
	m.Method("GET", pattern, handlerFn)
}

func (m *Mux) Post(pattern string, handlerFn http.HandlerFunc) {
	m.Method("POST", pattern, handlerFn)
}

func (m *Mux) ServeHTTP(w http.ResponseWriter, r *http.Request) {
	for _, rt := range m.routes {
		if !m.match(r.Method, rt.method) {
			continue
		}
		if !pathMatch(r.URL.Path, rt.pattern) {
			continue
		}
		handler := rt.handler
		for i := len(m.middlewares) - 1; i >= 0; i-- {
			handler = m.middlewares[i](handler)
		}
		handler.ServeHTTP(w, r)
		return
	}
	http.NotFound(w, r)
}

func (m *Mux) match(got, want string) bool {
	if want == "" {
		return true
	}
	return strings.EqualFold(got, want)
}

func pathMatch(path, pattern string) bool {
	if pattern == path {
		return true
	}
	if strings.HasSuffix(pattern, "/*") {
		prefix := strings.TrimSuffix(pattern, "/*")
		if strings.HasPrefix(path, prefix) {
			return true
		}
	}
	if strings.HasSuffix(pattern, "/") {
		return path == strings.TrimSuffix(pattern, "/") || strings.HasPrefix(path, pattern)
	}
	return false
}
