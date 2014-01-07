#include<stdlib.h>
#include"memory_manager.h"
#include"rational.h"
#include"prime_generator.h"
#include"debug.h"

// If PROFILE_MEMORY is defined, Z3 will display the amount of memory used, and the number of synchronization steps during finalization
// #define PROFILE_MEMORY
void initialize_symbols();
void finalize_symbols();

out_of_memory_error::out_of_memory_error():z3_error(ERR_MEMOUT) {
}

volatile bool g_memory_out_of_memory  = false;
long long  g_memory_alloc_size        = 0;
long long  g_memory_max_size          = 0;
long long  g_memory_max_used_size     = 0;
long long  g_memory_watermark         = 0;
bool       g_exit_when_out_of_memory  = false;
char const * g_out_of_memory_msg      = "ERROR: out of memory";

void memory::exit_when_out_of_memory(bool flag, char const * msg) {
    g_exit_when_out_of_memory = flag;
    if (flag && msg)
        g_out_of_memory_msg = msg;
}

static void throw_out_of_memory() {
    #pragma omp critical (z3_memory_manager) 
    {
        if (!g_memory_out_of_memory) {
            g_memory_out_of_memory = true;
        }
    }
    if (g_exit_when_out_of_memory) {
        std::cerr << g_out_of_memory_msg << "\n";
        exit(ERR_MEMOUT);
    }
    else {
        throw out_of_memory_error();
    }
}

#ifdef PROFILE_MEMORY
unsigned g_synch_counter = 0;
class mem_usage_report {
public:
    ~mem_usage_report() { 
        std::cerr << "(memory :max " << g_memory_max_used_size 
                  << " :final " << g_memory_alloc_size 
                  << " :synch " << g_synch_counter << ")" << std::endl; 
    }
};
mem_usage_report g_info;
#endif

void memory::initialize(size_t max_size) {
    g_memory_out_of_memory  = false;
    g_memory_max_size       = max_size;
    rational::initialize();
    initialize_symbols();
}

bool memory::is_out_of_memory() {
    bool r = false;
    #pragma omp critical (z3_memory_manager) 
    {
        r = g_memory_out_of_memory;
    }
    return r;
}

void memory::set_high_watermark(size_t watermak) {
    // This method is only safe to invoke at initialization time, that is, before the threads are created.
    g_memory_watermark = watermak;
}

bool memory::above_high_watermark() {
    if (g_memory_watermark == 0)
        return false;
    bool r;
    #pragma omp critical (z3_memory_manager) 
    {
        r = g_memory_watermark < g_memory_alloc_size;
    }
    return r;
}

void memory::set_max_size(size_t max_size) {
    // This method is only safe to invoke at initialization time, that is, before the threads are created.
    g_memory_max_size = max_size;
}

static bool g_finalizing = false;

void memory::finalize() {
    g_finalizing = true;
    finalize_debug();
    finalize_trace();
    finalize_symbols();
    rational::finalize();
    prime_iterator::finalize();
}


unsigned long long memory::get_allocation_size() {
    long long r;
    #pragma omp critical (z3_memory_manager) 
    {
        r = g_memory_alloc_size;
    }
    if (r < 0)
        r = 0;
    return r;
}

unsigned long long memory::get_max_used_memory() {
    unsigned long long r;
    #pragma omp critical (z3_memory_manager) 
    {
        r = g_memory_max_used_size;
    }
    return r;
}

void memory::display_max_usage(std::ostream & os) {
    unsigned long long mem = get_max_used_memory();
    os << "max. heap size:     " 
       << static_cast<double>(mem)/static_cast<double>(1024*1024) 
       << " Mbytes\n";
}

void memory::display_i_max_usage(std::ostream & os) {
    unsigned long long mem = get_max_used_memory();
    std::cout << "MEMORY " 
              << static_cast<double>(mem)/static_cast<double>(1024*1024) 
              << "\n";
}

void memory::deallocate(char const * file, int line, void * p) {
    deallocate(p);
    TRACE_CODE(if (!g_finalizing) TRACE("memory", tout << "dealloc " << std::hex << p << std::dec << " " << file << ":" << line << "\n";););
}

void * memory::allocate(char const* file, int line, char const* obj, size_t s) {
    void * r = allocate(s);
    TRACE("memory", tout << "alloc " << std::hex << r << std::dec << " " << file << ":" << line << " " << obj << " " << s << "\n";);
    return r;
}

#if defined(_WINDOWS) || defined(_USE_THREAD_LOCAL)
// ==================================
// ==================================
// THREAD LOCAL VERSION
// ==================================
// ==================================


// We only integrate the local thread counters with the global one
// when the local counter > SYNCH_THRESHOLD 
#define SYNCH_THRESHOLD 100000

#ifdef _WINDOWS
// Actually this is VS specific instead of Windows specific.
__declspec(thread) long long g_memory_thread_alloc_size    = 0;
#else
// GCC style
__thread long long g_memory_thread_alloc_size    = 0;
#endif

static void synchronize_counters(bool allocating) {
#ifdef PROFILE_MEMORY
    g_synch_counter++;
#endif

    bool out_of_mem = false;
    #pragma omp critical (z3_memory_manager) 
    {
        g_memory_alloc_size += g_memory_thread_alloc_size;
        if (g_memory_alloc_size > g_memory_max_used_size)
            g_memory_max_used_size = g_memory_alloc_size;
        if (g_memory_max_size != 0 && g_memory_alloc_size > g_memory_max_size)
            out_of_mem = true;
    }
    g_memory_thread_alloc_size = 0;
    if (out_of_mem && allocating) {
        throw_out_of_memory();
    }
}

void memory::deallocate(void * p) {
    size_t * sz_p  = reinterpret_cast<size_t*>(p) - 1;
    size_t sz      = *sz_p;
    void * real_p  = reinterpret_cast<void*>(sz_p);
    g_memory_thread_alloc_size -= sz;
    free(real_p);
    if (g_memory_thread_alloc_size < -SYNCH_THRESHOLD) {
        synchronize_counters(false);
    }
}

void * memory::allocate(size_t s) {
    if (s == 0) 
        return 0;
    s = s + sizeof(size_t); // we allocate an extra field!
    void * r = malloc(s);
    if (r == 0) {
        throw_out_of_memory();
    }
    *(static_cast<size_t*>(r)) = s;
    g_memory_thread_alloc_size += s;
    if (g_memory_thread_alloc_size > SYNCH_THRESHOLD) {
        synchronize_counters(true);
    }
    return static_cast<size_t*>(r) + 1; // we return a pointer to the location after the extra field
}

#else
// ==================================
// ==================================
// NO THREAD LOCAL VERSION
// ==================================
// ==================================
// allocate & deallocate without using thread local storage

void memory::deallocate(void * p) {
    size_t * sz_p  = reinterpret_cast<size_t*>(p) - 1;
    size_t sz      = *sz_p;
    void * real_p  = reinterpret_cast<void*>(sz_p);
    #pragma omp critical (z3_memory_manager) 
    {
        g_memory_alloc_size -= sz;
    }
    free(real_p);
}

void * memory::allocate(size_t s) {
    if (s == 0) 
        return 0;
    s = s + sizeof(size_t); // we allocate an extra field!
    bool out_of_mem = false;
    #pragma omp critical (z3_memory_manager) 
    {
        g_memory_alloc_size += s;
        if (g_memory_alloc_size > g_memory_max_used_size)
            g_memory_max_used_size = g_memory_alloc_size;
        if (g_memory_max_size != 0 && g_memory_alloc_size > g_memory_max_size)
            out_of_mem = true;
    }
    if (out_of_mem)
        throw_out_of_memory();
    void * r = malloc(s);
    if (r == 0)
        throw_out_of_memory();
    *(static_cast<size_t*>(r)) = s;
    return static_cast<size_t*>(r) + 1; // we return a pointer to the location after the extra field
}
 
#endif
