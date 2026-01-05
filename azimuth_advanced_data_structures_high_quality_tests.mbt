// Azimuth Telemetry System - Advanced Data Structures High-Quality Tests
// This file contains comprehensive test cases for advanced data structures used in telemetry

// Test 1: Circular Buffer Operations
test "circular buffer operations" {
  let buffer = CircularBuffer::new(5)
  
  // Test empty buffer
  assert_true(CircularBuffer::is_empty(buffer))
  assert_false(CircularBuffer::is_full(buffer))
  assert_eq(CircularBuffer::size(buffer), 0)
  
  // Test adding elements
  CircularBuffer::push(buffer, 1)
  CircularBuffer::push(buffer, 2)
  CircularBuffer::push(buffer, 3)
  
  assert_false(CircularBuffer::is_empty(buffer))
  assert_false(CircularBuffer::is_full(buffer))
  assert_eq(CircularBuffer::size(buffer), 3)
  
  // Test removing elements
  let first = CircularBuffer::pop(buffer)
  match first {
    Some(value) => assert_eq(value, 1)
    None => assert_true(false)
  }
  
  assert_eq(CircularBuffer::size(buffer), 2)
  
  // Test filling buffer to capacity
  CircularBuffer::push(buffer, 4)
  CircularBuffer::push(buffer, 5)
  CircularBuffer::push(buffer, 6) // This should overwrite the first element
  
  assert_true(CircularBuffer::is_full(buffer))
  assert_eq(CircularBuffer::size(buffer), 5)
  
  // Test buffer iteration
  let mut sum = 0
  CircularBuffer::for_each(buffer, fn(x) { sum = sum + x })
  assert_eq(sum, 20) // 2 + 3 + 4 + 5 + 6
}

// Test 2: Priority Queue Operations
test "priority queue operations" {
  let queue = PriorityQueue::new()
  
  // Test empty queue
  assert_true(PriorityQueue::is_empty(queue))
  assert_eq(PriorityQueue::size(queue), 0)
  
  // Test inserting elements with priorities
  PriorityQueue::insert(queue, 10, 1) // value, priority
  PriorityQueue::insert(queue, 20, 3)
  PriorityQueue::insert(queue, 15, 2)
  PriorityQueue::insert(queue, 5, 0)
  
  assert_false(PriorityQueue::is_empty(queue))
  assert_eq(PriorityQueue::size(queue), 4)
  
  // Test extracting elements (should come out in priority order)
  let first = PriorityQueue::extract_max(queue)
  match first {
    Some(value) => assert_eq(value, 20) // Highest priority
    None => assert_true(false)
  }
  
  let second = PriorityQueue::extract_max(queue)
  match second {
    Some(value) => assert_eq(value, 15) // Second highest
    None => assert_true(false)
  }
  
  assert_eq(PriorityQueue::size(queue), 2)
  
  // Test peek at max element
  let max = PriorityQueue::peek_max(queue)
  match max {
    Some(value) => assert_eq(value, 10)
    None => assert_true(false)
  }
}

// Test 3: Trie (Prefix Tree) Operations
test "trie prefix tree operations" {
  let trie = Trie::new()
  
  // Test empty trie
  assert_false(Trie::contains(trie, "test"))
  assert_eq(Trie::count_prefixes(trie, "test"), 0)
  
  // Test inserting words
  Trie::insert(trie, "hello")
  Trie::insert(trie, "help")
  Trie::insert(trie, "helmet")
  Trie::insert(trie, "world")
  
  // Test word containment
  assert_true(Trie::contains(trie, "hello"))
  assert_true(Trie::contains(trie, "help"))
  assert_false(Trie::contains(trie, "hell")) // Not inserted as a complete word
  
  // Test prefix counting
  assert_eq(Trie::count_prefixes(trie, "hel"), 3) // hello, help, helmet
  assert_eq(Trie::count_prefixes(trie, "he"), 3)
  assert_eq(Trie::count_prefixes(trie, "w"), 1) // world
  assert_eq(Trie::count_prefixes(trie, "x"), 0)
  
  // Test word removal
  Trie::remove(trie, "help")
  assert_false(Trie::contains(trie, "help"))
  assert_true(Trie::contains(trie, "hello"))
  assert_eq(Trie::count_prefixes(trie, "hel"), 2) // hello, helmet
}

// Test 4: Bloom Filter Operations
test "bloom filter operations" {
  let filter = BloomFilter::new(1000, 3) // capacity, hash_count
  
  // Test empty filter
  assert_false(BloomFilter::might_contain(filter, "test"))
  
  // Test adding elements
  BloomFilter::add(filter, "hello")
  BloomFilter::add(filter, "world")
  BloomFilter::add(filter, "test")
  
  // Test element presence (might have false positives)
  assert_true(BloomFilter::might_contain(filter, "hello"))
  assert_true(BloomFilter::might_contain(filter, "world"))
  assert_true(BloomFilter::might_contain(filter, "test"))
  
  // Test non-existent element (might have false positives)
  // This test demonstrates the probabilistic nature
  let might_contain = BloomFilter::might_contain(filter, "nonexistent")
  // We can't assert false here due to possible false positives
  
  // Test filter statistics
  let false_positive_rate = BloomFilter::estimated_false_positive_rate(filter)
  assert_true(false_positive_rate >= 0.0)
  assert_true(false_positive_rate <= 1.0)
}

// Test 5: LRU Cache Operations
test "lru cache operations" {
  let cache = LRUCache::new(3) // capacity
  
  // Test empty cache
  assert_eq(LRUCache::size(cache), 0)
  assert_false(LRUCache::contains(cache, "key1"))
  
  // Test inserting elements
  LRUCache::put(cache, "key1", "value1")
  LRUCache::put(cache, "key2", "value2")
  LRUCache::put(cache, "key3", "value3")
  
  assert_eq(LRUCache::size(cache), 3)
  assert_true(LRUCache::contains(cache, "key1"))
  assert_true(LRUCache::contains(cache, "key2"))
  assert_true(LRUCache::contains(cache, "key3"))
  
  // Test getting elements
  let value1 = LRUCache::get(cache, "key1")
  match value1 {
    Some(v) => assert_eq(v, "value1")
    None => assert_true(false)
  }
  
  // Test LRU eviction
  LRUCache::put(cache, "key4", "value4") // Should evict key2 (least recently used)
  
  assert_eq(LRUCache::size(cache), 3)
  assert_true(LRUCache::contains(cache, "key1")) // Recently accessed
  assert_false(LRUCache::contains(cache, "key2")) // Should be evicted
  assert_true(LRUCache::contains(cache, "key3"))
  assert_true(LRUCache::contains(cache, "key4"))
  
  // Test updating existing key
  LRUCache::put(cache, "key1", "new_value1")
  let updated_value = LRUCache::get(cache, "key1")
  match updated_value {
    Some(v) => assert_eq(v, "new_value1")
    None => assert_true(false)
  }
}

// Test 6: Skip List Operations
test "skip list operations" {
  let skip_list = SkipList::new()
  
  // Test empty skip list
  assert_true(SkipList::is_empty(skip_list))
  assert_eq(SkipList::size(skip_list), 0)
  assert_false(SkipList::contains(skip_list, 10))
  
  // Test inserting elements
  SkipList::insert(skip_list, 30)
  SkipList::insert(skip_list, 10)
  SkipList::insert(skip_list, 50)
  SkipList::insert(skip_list, 20)
  SkipList::insert(skip_list, 40)
  
  assert_false(SkipList::is_empty(skip_list))
  assert_eq(SkipList::size(skip_list), 5)
  
  // Test element containment
  assert_true(SkipList::contains(skip_list, 10))
  assert_true(SkipList::contains(skip_list, 20))
  assert_true(SkipList::contains(skip_list, 30))
  assert_true(SkipList::contains(skip_list, 40))
  assert_true(SkipList::contains(skip_list, 50))
  assert_false(SkipList::contains(skip_list, 60))
  
  // Test range queries
  let range_result = SkipList::range_query(skip_list, 20, 40)
  assert_eq(range_result.length(), 3) // 20, 30, 40
  
  // Test deletion
  SkipList::delete(skip_list, 30)
  assert_false(SkipList::contains(skip_list, 30))
  assert_eq(SkipList::size(skip_list), 4)
}

// Test 7: Union-Find (Disjoint Set) Operations
test "union-find disjoint set operations" {
  let uf = UnionFind::new(5) // 5 elements (0-4)
  
  // Test initial state
  assert_false(UnionFind::connected(uf, 0, 1))
  assert_eq(UnionFind::find(uf, 0), 0)
  assert_eq(UnionFind::find(uf, 1), 1)
  
  // Test union operations
  UnionFind::union(uf, 0, 1)
  assert_true(UnionFind::connected(uf, 0, 1))
  assert_eq(UnionFind::find(uf, 0), UnionFind::find(uf, 1))
  
  UnionFind::union(uf, 2, 3)
  assert_true(UnionFind::connected(uf, 2, 3))
  assert_false(UnionFind::connected(uf, 1, 2))
  
  // Test transitive connections
  UnionFind::union(uf, 1, 2)
  assert_true(UnionFind::connected(uf, 0, 3)) // 0-1-2-3 are connected
  
  // Test component count
  assert_eq(UnionFind::count_components(uf), 2) // {0,1,2,3} and {4}
}

// Test 8: Segment Tree Operations
test "segment tree operations" {
  let arr = [1, 2, 3, 4, 5]
  let seg_tree = SegmentTree::new(arr, fn(a, b) { a + b }) // Sum segment tree
  
  // Test range sum queries
  assert_eq(SegmentTree::query(seg_tree, 0, 4), 15) // Sum of all elements
  assert_eq(SegmentTree::query(seg_tree, 1, 3), 9)  // 2 + 3 + 4
  assert_eq(SegmentTree::query(seg_tree, 2, 2), 3)  // Single element
  
  // Test point updates
  SegmentTree::update(seg_tree, 2, 10) // Change arr[2] from 3 to 10
  assert_eq(SegmentTree::query(seg_tree, 0, 4), 22) // New sum
  assert_eq(SegmentTree::query(seg_tree, 1, 3), 16)  // 2 + 10 + 4
  
  // Test edge cases
  assert_eq(SegmentTree::query(seg_tree, 0, 0), 1)  // First element
  assert_eq(SegmentTree::query(seg_tree, 4, 4), 5)  // Last element
}

// Test 9: Fenwick Tree (Binary Indexed Tree) Operations
test "fenwick tree operations" {
  let arr = [1, 2, 3, 4, 5]
  let fenwick = FenwickTree::new(arr)
  
  // Test prefix sum queries
  assert_eq(FenwickTree::prefix_sum(fenwick, 0), 1)
  assert_eq(FenwickTree::prefix_sum(fenwick, 1), 3)  // 1 + 2
  assert_eq(FenwickTree::prefix_sum(fenwick, 2), 6)  // 1 + 2 + 3
  assert_eq(FenwickTree::prefix_sum(fenwick, 4), 15) // Sum of all elements
  
  // Test range sum queries
  assert_eq(FenwickTree::range_sum(fenwick, 1, 3), 9)  // 2 + 3 + 4
  assert_eq(FenwickTree::range_sum(fenwick, 0, 2), 6)  // 1 + 2 + 3
  
  // Test point updates
  FenwickTree::update(fenwick, 2, 7) // Add 7 to arr[2] (3 -> 10)
  assert_eq(FenwickTree::prefix_sum(fenwick, 2), 13) // 1 + 2 + 10
  assert_eq(FenwickTree::prefix_sum(fenwick, 4), 22) // New total sum
}

// Test 10: Suffix Array Operations
test "suffix array operations" {
  let text = "banana"
  let suffix_array = SuffixArray::new(text)
  
  // Test suffix array properties
  assert_eq(SuffixArray::size(suffix_array), 6)
  
  // Test lexicographic order of suffixes
  // The suffixes of "banana" in order are:
  // "a", "ana", "anana", "banana", "na", "nana"
  let first_suffix = SuffixArray::get_suffix(suffix_array, 0)
  assert_eq(first_suffix, "a")
  
  let last_suffix = SuffixArray::get_suffix(suffix_array, 5)
  assert_eq(last_suffix, "nana")
  
  // Test pattern searching
  let occurrences = SuffixArray::find_occurrences(suffix_array, "ana")
  assert_eq(occurrences.length(), 2) // "ana" appears twice
  
  // Test longest common prefix queries
  let lcp = SuffixArray::lcp(suffix_array, 1, 3) // LCP of "ana" and "banana"
  assert_eq(lcp, 0) // No common prefix
  
  let lcp2 = SuffixArray::lcp(suffix_array, 0, 1) // LCP of "a" and "ana"
  assert_eq(lcp2, 1) // Common prefix is "a"
}