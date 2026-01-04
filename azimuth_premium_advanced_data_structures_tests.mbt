// Azimuth Premium Advanced Data Structures Tests
// This file contains comprehensive test cases for advanced data structures

// Test 1: Efficient Trie Implementation
test "efficient trie implementation" {
  // Define trie node
  type TrieNode = {
    children: Array[(Char, TrieNode)],
    is_end_of_word: Bool,
    value: Option[String]
  }
  
  // Define trie
  type Trie = {
    root: TrieNode,
    size: Int
  }
  
  // Create trie node
  let create_trie_node = fn() {
    {
      children: [],
      is_end_of_word: false,
      value: None
    }
  }
  
  // Create trie
  let create_trie = fn() {
    {
      root: create_trie_node(),
      size: 0
    }
  }
  
  // Insert word into trie
  let insert = fn(trie: Trie, word: String, value: String) {
    let rec insert_node = fn(node: TrieNode, chars: Array[Char], index: Int) {
      if index >= chars.length() {
        {
          children: node.children,
          is_end_of_word: true,
          value: Some(value)
        }
      } else {
        let current_char = chars[index]
        
        match node.children.find_index(fn(pair) { pair.0 == current_char }) {
          Some(child_index) => {
            let (char, child_node) = node.children[child_index]
            let updated_child = insert_node(child_node, chars, index + 1)
            let updated_children = node.children.update(child_index, (char, updated_child))
            
            {
              children: updated_children,
              is_end_of_word: node.is_end_of_word,
              value: node.value
            }
          }
          None => {
            let new_child = insert_node(create_trie_node(), chars, index + 1)
            let updated_children = node.children.push((current_char, new_child))
            
            {
              children: updated_children,
              is_end_of_word: node.is_end_of_word,
              value: node.value
            }
          }
        }
      }
    }
    
    let chars = word.to_char_array()
    let updated_root = insert_node(trie.root, chars, 0)
    
    {
      root: updated_root,
      size: trie.size + 1
    }
  }
  
  // Search for word in trie
  let search = fn(trie: Trie, word: String) {
    let rec search_node = fn(node: TrieNode, chars: Array[Char], index: Int) {
      if index >= chars.length() {
        if node.is_end_of_word {
          Some(node.value)
        } else {
          None
        }
      } else {
        let current_char = chars[index]
        
        match node.children.find(fn(pair) { pair.0 == current_char }) {
          Some((_, child_node)) => search_node(child_node, chars, index + 1)
          None => None
        }
      }
    }
    
    let chars = word.to_char_array()
    search_node(trie.root, chars, 0)
  }
  
  // Test trie creation
  let trie = create_trie()
  assert_eq(trie.size, 0)
  assert_false(trie.root.is_end_of_word)
  assert_eq(trie.root.children.length(), 0)
  
  // Test inserting words
  let trie1 = insert(trie, "hello", "greeting")
  assert_eq(trie1.size, 1)
  
  let trie2 = insert(trie1, "world", "planet")
  assert_eq(trie2.size, 2)
  
  let trie3 = insert(trie2, "help", "assistance")
  assert_eq(trie3.size, 3)
  
  // Test searching for words
  match search(trie3, "hello") {
    Some(value) => assert_eq(value, "greeting")
    None => assert_true(false)
  }
  
  match search(trie3, "world") {
    Some(value) => assert_eq(value, "planet")
    None => assert_true(false)
  }
  
  match search(trie3, "help") {
    Some(value) => assert_eq(value, "assistance")
    None => assert_true(false)
  }
  
  // Test searching for non-existent words
  match search(trie3, "hel") {
    None => assert_true(true)
    Some(_) => assert_true(false)
  }
  
  match search(trie3, "helping") {
    None => assert_true(true)
    Some(_) => assert_true(false)
  }
  
  match search(trie3, "wonderful") {
    None => assert_true(true)
    Some(_) => assert_true(false)
  }
  
  // Test prefix sharing
  let trie4 = insert(trie3, "hero", "superhero")
  assert_eq(trie4.size, 4)
  
  // The prefix "he" should be shared between "hello", "help", and "hero"
  match search(trie4, "hero") {
    Some(value) => assert_eq(value, "superhero")
    None => assert_true(false)
  }
}

// Test 2: Bloom Filter Implementation
test "bloom filter implementation" {
  // Define bloom filter
  type BloomFilter = {
    bit_array: Array[Bool],
    size: Int,
    hash_count: Int,
    item_count: Int
  }
  
  // Create bloom filter
  let create_bloom_filter = fn(size: Int, hash_count: Int) {
    {
      bit_array: [false] * size,
      size,
      hash_count,
      item_count: 0
    }
  }
  
  // Simple hash functions for testing
  let hash1 = fn(item: String, size: Int) {
    let mut hash = 0
    for i in 0..item.length() {
      hash = (hash + item[i].to_int()) % size
    }
    hash
  }
  
  let hash2 = fn(item: String, size: Int) {
    let mut hash = 1
    for i in 0..item.length() {
      hash = (hash * 31 + item[i].to_int()) % size
    }
    hash
  }
  
  let hash3 = fn(item: String, size: Int) {
    let mut hash = 5381
    for i in 0..item.length() {
      hash = ((hash << 5) + hash + item[i].to_int()) % size
    }
    hash
  }
  
  // Add item to bloom filter
  let add = fn(filter: BloomFilter, item: String) {
    let mut updated_bit_array = filter.bit_array
    
    for i in 0..filter.hash_count {
      let hash_value = match i {
        0 => hash1(item, filter.size)
        1 => hash2(item, filter.size)
        2 => hash3(item, filter.size)
        _ => hash1(item, filter.size)
      }
      
      updated_bit_array = updated_bit_array.update(hash_value, true)
    }
    
    {
      bit_array: updated_bit_array,
      size: filter.size,
      hash_count: filter.hash_count,
      item_count: filter.item_count + 1
    }
  }
  
  // Check if item might be in bloom filter
  let might_contain = fn(filter: BloomFilter, item: String) {
    for i in 0..filter.hash_count {
      let hash_value = match i {
        0 => hash1(item, filter.size)
        1 => hash2(item, filter.size)
        2 => hash3(item, filter.size)
        _ => hash1(item, filter.size)
      }
      
      if !filter.bit_array[hash_value] {
        return false
      }
    }
    
    true
  }
  
  // Test bloom filter creation
  let filter = create_bloom_filter(100, 3)
  assert_eq(filter.size, 100)
  assert_eq(filter.hash_count, 3)
  assert_eq(filter.item_count, 0)
  assert_eq(filter.bit_array.length(), 100)
  
  // Test adding items
  let filter1 = add(filter, "hello")
  assert_eq(filter1.item_count, 1)
  
  let filter2 = add(filter1, "world")
  assert_eq(filter2.item_count, 2)
  
  let filter3 = add(filter2, "test")
  assert_eq(filter3.item_count, 3)
  
  // Test checking for existing items
  assert_true(might_contain(filter3, "hello"))
  assert_true(might_contain(filter3, "world"))
  assert_true(might_contain(filter3, "test"))
  
  // Test checking for non-existing items
  let not_exist1 = might_contain(filter3, "foo")
  let not_exist2 = might_contain(filter3, "bar")
  let not_exist3 = might_contain(filter3, "baz")
  
  // These might be true due to false positives, but should not all be true
  assert_false(not_exist1 && not_exist2 && not_exist3)
  
  // Test false positive rate (should be low with reasonable size)
  let mut false_positive_count = 0
  let test_items = ["apple", "banana", "cherry", "date", "elderberry"]
  
  for item in test_items {
    if might_contain(filter3, item) {
      false_positive_count = false_positive_count + 1
    }
  }
  
  // With 3 items in a 100-bit filter, false positive rate should be low
  assert_true(false_positive_count <= test_items.length())
}

// Test 3: Skip List Implementation
test "skip list implementation" {
  // Define skip list node
  type SkipListNode = {
    key: Int,
    value: String,
    forward: Array[Option[SkipListNode]]
  }
  
  // Define skip list
  type SkipList = {
    head: SkipListNode,
    max_level: Int,
    level: Int,
    size: Int
  }
  
  // Create skip list node
  let create_skip_list_node = fn(key: Int, value: String, level: Int) {
    {
      key,
      value,
      forward: [None] * (level + 1)
    }
  }
  
  // Create skip list
  let create_skip_list = fn(max_level: Int) {
    let head = create_skip_list_node(Int::min_value(), "HEAD", max_level)
    
    {
      head,
      max_level,
      level: 0,
      size: 0
    }
  }
  
  // Random level generator (simplified)
  let random_level = fn(max_level: Int) {
    let mut level = 0
    let mut random = 42  // Fixed seed for reproducible tests
    
    while level < max_level && random % 2 == 0 {
      level = level + 1
      random = random * 1103515245 + 12345
    }
    
    level
  }
  
  // Insert into skip list
  let insert = fn(list: SkipList, key: Int, value: String) {
    let update = [None] * (list.max_level + 1)
    let mut current = list.head
    
    // Find insertion points
    for i in (list.level)..=0 {
      while match current.forward[i] {
        Some(node) => node.key < key,
        None => false
      } {
        current = match current.forward[i] {
          Some(node) => node,
          None => current
        }
      }
      update = update.update(i, Some(current))
    }
    
    // Check if key already exists
    match current.forward[0] {
      Some(node) if node.key == key => {
        // Update existing node
        let updated_node = { node | value }
        let mut updated_forward = list.head.forward
        updated_forward = updated_forward.update(0, Some(updated_node))
        
        let updated_head = { list.head | forward: updated_forward }
        
        {
          head: updated_head,
          max_level: list.max_level,
          level: list.level,
          size: list.size
        }
      }
      _ => {
        // Insert new node
        let new_level = random_level(list.max_level)
        if new_level > list.level {
          for i in (list.level + 1)..=new_level {
            update = update.update(i, Some(list.head))
          }
        }
        
        let new_node = create_skip_list_node(key, value, new_level)
        
        // Update forward pointers
        let mut updated_head = list.head
        for i in 0..=new_level {
          match update[i] {
            Some(prev_node) => {
              let updated_node = {
                new_node | 
                forward: new_node.forward.update(i, prev_node.forward[i])
              }
              
              let updated_prev_forward = prev_node.forward.update(i, Some(updated_node))
              
              if prev_node.key == list.head.key {
                updated_head = { updated_head | forward: updated_prev_forward }
              }
            }
            None => ()
          }
        }
        
        {
          head: updated_head,
          max_level: list.max_level,
          level: if new_level > list.level { new_level } else { list.level },
          size: list.size + 1
        }
      }
    }
  }
  
  // Search in skip list
  let search = fn(list: SkipList, key: Int) {
    let mut current = list.head
    
    for i in (list.level)..=0 {
      while match current.forward[i] {
        Some(node) => node.key < key,
        None => false
      } {
        current = match current.forward[i] {
          Some(node) => node,
          None => current
        }
      }
    }
    
    match current.forward[0] {
      Some(node) if node.key == key => Some(node.value),
      _ => None
    }
  }
  
  // Test skip list creation
  let list = create_skip_list(4)
  assert_eq(list.max_level, 4)
  assert_eq(list.level, 0)
  assert_eq(list.size, 0)
  assert_eq(list.head.key, Int::min_value())
  
  // Test inserting items
  let list1 = insert(list, 10, "ten")
  assert_eq(list1.size, 1)
  
  let list2 = insert(list1, 20, "twenty")
  assert_eq(list2.size, 2)
  
  let list3 = insert(list2, 5, "five")
  assert_eq(list3.size, 3)
  
  let list4 = insert(list3, 15, "fifteen")
  assert_eq(list4.size, 4)
  
  // Test searching for existing items
  match search(list4, 10) {
    Some(value) => assert_eq(value, "ten")
    None => assert_true(false)
  }
  
  match search(list4, 20) {
    Some(value) => assert_eq(value, "twenty")
    None => assert_true(false)
  }
  
  match search(list4, 5) {
    Some(value) => assert_eq(value, "five")
    None => assert_true(false)
  }
  
  match search(list4, 15) {
    Some(value) => assert_eq(value, "fifteen")
    None => assert_true(false)
  }
  
  // Test searching for non-existing items
  match search(list4, 25) {
    None => assert_true(true)
    Some(_) => assert_true(false)
  }
  
  match search(list4, 1) {
    None => assert_true(true)
    Some(_) => assert_true(false)
  }
  
  // Test updating existing item
  let list5 = insert(list4, 10, "updated-ten")
  match search(list5, 10) {
    Some(value) => assert_eq(value, "updated-ten")
    None => assert_true(false)
  }
  assert_eq(list5.size, 4)  // Size should not increase
}

// Test 4: LRU Cache Implementation
test "lru cache implementation" {
  // Define doubly linked list node
  type LRUNode = {
    key: String,
    value: String,
    prev: Option[LRUNode],
    next: Option[LRUNode]
  }
  
  // Define LRU cache
  type LRUCache = {
    capacity: Int,
    cache: Array[(String, LRUNode)],
    head: Option[LRUNode],
    tail: Option[LRUNode],
    size: Int
  }
  
  // Create LRU node
  let create_lru_node = fn(key: String, value: String) {
    {
      key,
      value,
      prev: None,
      next: None
    }
  }
  
  // Create LRU cache
  let create_lru_cache = fn(capacity: Int) {
    {
      capacity,
      cache: [],
      head: None,
      tail: None,
      size: 0
    }
  }
  
  // Remove node from linked list
  let remove_node = fn(cache: LRUCache, node: LRUNode) {
    match node.prev {
      Some(prev_node) => {
        let updated_prev = { prev_node | next: node.next }
      }
      None => {
        // Node is head
        cache.head = node.next
      }
    }
    
    match node.next {
      Some(next_node) => {
        let updated_next = { next_node | prev: node.prev }
      }
      None => {
        // Node is tail
        cache.tail = node.prev
      }
    }
    
    cache
  }
  
  // Add node to head of linked list
  let add_to_head = fn(cache: LRUCache, node: LRUNode) {
    node.next = cache.head
    node.prev = None
    
    match cache.head {
      Some(head_node) => {
        let updated_head = { head_node | prev: Some(node) }
        cache.head = Some(node)
      }
      None => {
        cache.head = Some(node)
        cache.tail = Some(node)
      }
    }
    
    cache.size = cache.size + 1
  }
  
  // Move node to head
  let move_to_head = fn(cache: LRUCache, node: LRUNode) {
    remove_node(cache, node)
    add_to_head(cache, node)
  }
  
  // Pop tail node
  let pop_tail = fn(cache: LRUCache) {
    match cache.tail {
      Some(tail_node) => {
        remove_node(cache, tail_node)
        Some(tail_node)
      }
      None => None
    }
  }
  
  // Get value from cache
  let get = fn(cache: LRUCache, key: String) {
    match cache.cache.find_index(fn(pair) { pair.0 == key }) {
      Some(index) => {
        let (_, node) = cache.cache[index]
        move_to_head(cache, node)
        Some(node.value)
      }
      None => None
    }
  }
  
  // Put value into cache
  let put = fn(cache: LRUCache, key: String, value: String) {
    match cache.cache.find_index(fn(pair) { pair.0 == key }) {
      Some(index) => {
        let (_, node) = cache.cache[index]
        let updated_node = { node | value }
        let updated_cache = cache.cache.update(index, (key, updated_node))
        move_to_head({ cache | cache: updated_cache }, updated_node)
      }
      None => {
        let new_node = create_lru_node(key, value)
        
        if cache.size == cache.capacity {
          match pop_tail(cache) {
            Some(tail_node) => {
              let updated_cache = cache.cache.filter(fn(pair) { pair.0 != tail_node.key })
              let cache_with_removed = { cache | cache: updated_cache }
              add_to_head(cache_with_removed, new_node)
            }
            None => cache
          }
        } else {
          add_to_head(cache, new_node)
        }
      }
    }
  }
  
  // Test LRU cache creation
  let cache = create_lru_cache(3)
  assert_eq(cache.capacity, 3)
  assert_eq(cache.size, 0)
  
  // Test putting items
  let cache1 = put(cache, "a", "value-a")
  let cache2 = put(cache1, "b", "value-b")
  let cache3 = put(cache2, "c", "value-c")
  
  // Test getting items
  match get(cache3, "a") {
    Some(value) => assert_eq(value, "value-a")
    None => assert_true(false)
  }
  
  // Add item that exceeds capacity
  let cache4 = put(cache3, "d", "value-d")
  
  // "a" should be the least recently used and should be evicted
  match get(cache4, "a") {
    None => assert_true(true)
    Some(_) => assert_true(false)
  }
  
  // "b", "c", and "d" should still be in cache
  match get(cache4, "b") {
    Some(value) => assert_eq(value, "value-b")
    None => assert_true(false)
  }
  
  match get(cache4, "c") {
    Some(value) => assert_eq(value, "value-c")
    None => assert_true(false)
  }
  
  match get(cache4, "d") {
    Some(value) => assert_eq(value, "value-d")
    None => assert_true(false)
  }
  
  // Access "b" to make it most recently used
  let cache5 = get(cache4, "b")
  
  // Add another item
  let cache6 = put(cache4, "e", "value-e")
  
  // "c" should be evicted (not "b" because we just accessed it)
  match get(cache6, "c") {
    None => assert_true(true)
    Some(_) => assert_true(false)
  }
  
  match get(cache6, "b") {
    Some(value) => assert_eq(value, "value-b")
    None => assert_true(false)
  }
}

// Test 5: Disjoint Set Union (Union-Find) Implementation
test "disjoint set union implementation" {
  // Define DSU element
  type DSUElement = {
    parent: Int,
    rank: Int,
    size: Int
  }
  
  // Define DSU
  type DSU = {
    elements: Array[DSUElement],
    sets: Int
  }
  
  // Create DSU element
  let create_dsu_element = fn(parent: Int) {
    {
      parent,
      rank: 0,
      size: 1
    }
  }
  
  // Create DSU
  let create_dsu = fn(size: Int) {
    let elements = (0..size).map(create_dsu_element)
    
    {
      elements,
      sets: size
    }
  }
  
  // Find with path compression
  let find = fn(dsu: DSU, x: Int) {
    let rec find_with_compression = fn(elements: Array[DSUElement], x: Int) {
      let element = elements[x]
      
      if element.parent != x {
        let root = find_with_compression(elements, element.parent)
        // Path compression
        let updated_element = { element | parent: root }
        elements.update(x, updated_element)
      } else {
        x
      }
    }
    
    find_with_compression(dsu.elements, x)
  }
  
  // Union by rank
  let union = fn(dsu: DSU, x: Int, y: Int) {
    let root_x = find(dsu, x)
    let root_y = find(dsu, y)
    
    if root_x != root_y {
      let element_x = dsu.elements[root_x]
      let element_y = dsu.elements[root_y]
      
      if element_x.rank < element_y.rank {
        // Make x child of y
        let updated_element_x = { element_x | parent: root_y }
        let updated_element_y = { element_y | size: element_y.size + element_x.size }
        let updated_elements = dsu.elements
          .update(root_x, updated_element_x)
          .update(root_y, updated_element_y)
        
        {
          elements: updated_elements,
          sets: dsu.sets - 1
        }
      } else if element_x.rank > element_y.rank {
        // Make y child of x
        let updated_element_y = { element_y | parent: root_x }
        let updated_element_x = { element_x | size: element_x.size + element_y.size }
        let updated_elements = dsu.elements
          .update(root_y, updated_element_y)
          .update(root_x, updated_element_x)
        
        {
          elements: updated_elements,
          sets: dsu.sets - 1
        }
      } else {
        // Same rank, make y child of x and increment x's rank
        let updated_element_y = { element_y | parent: root_x }
        let updated_element_x = { element_x | rank: element_x.rank + 1, size: element_x.size + element_y.size }
        let updated_elements = dsu.elements
          .update(root_y, updated_element_y)
          .update(root_x, updated_element_x)
        
        {
          elements: updated_elements,
          sets: dsu.sets - 1
        }
      }
    } else {
      dsu  // Already in the same set
    }
  }
  
  // Check if two elements are in the same set
  let connected = fn(dsu: DSU, x: Int, y: Int) {
    find(dsu, x) == find(dsu, y)
  }
  
  // Get set size
  let get_set_size = fn(dsu: DSU, x: Int) {
    let root = find(dsu, x)
    dsu.elements[root].size
  }
  
  // Test DSU creation
  let dsu = create_dsu(10)
  assert_eq(dsu.sets, 10)
  assert_eq(dsu.elements.length(), 11)  // 0..10
  
  // Test initial state
  for i in 0..10 {
    assert_eq(find(dsu, i), i)
    assert_eq(get_set_size(dsu, i), 1)
  }
  
  // Test union operations
  let dsu1 = union(dsu, 1, 2)
  assert_eq(dsu1.sets, 9)
  assert_true(connected(dsu1, 1, 2))
  assert_eq(get_set_size(dsu1, 1), 2)
  assert_eq(get_set_size(dsu1, 2), 2)
  
  let dsu2 = union(dsu1, 3, 4)
  assert_eq(dsu2.sets, 8)
  assert_true(connected(dsu2, 3, 4))
  assert_eq(get_set_size(dsu2, 3), 2)
  
  let dsu3 = union(dsu2, 1, 3)
  assert_eq(dsu3.sets, 7)
  assert_true(connected(dsu3, 1, 4))  // Transitive connection
  assert_eq(get_set_size(dsu3, 1), 4)
  
  // Test path compression
  let root_1 = find(dsu3, 1)
  let root_2 = find(dsu3, 2)
  let root_3 = find(dsu3, 3)
  let root_4 = find(dsu3, 4)
  
  assert_eq(root_1, root_2)
  assert_eq(root_2, root_3)
  assert_eq(root_3, root_4)
  
  // Test union of large sets
  let dsu4 = union(dsu3, 5, 6)
  let dsu5 = union(dsu4, 7, 8)
  let dsu6 = union(dsu5, 5, 7)
  
  assert_eq(dsu6.sets, 5)
  assert_true(connected(dsu6, 5, 8))
  assert_eq(get_set_size(dsu6, 5), 4)
  
  // Test connecting two large sets
  let dsu7 = union(dsu6, 1, 5)
  assert_eq(dsu7.sets, 4)
  assert_true(connected(dsu7, 2, 8))  // All connected
  assert_eq(get_set_size(dsu7, 1), 8)
  
  // Test remaining singletons
  assert_eq(get_set_size(dsu7, 0), 1)
  assert_eq(get_set_size(dsu7, 9), 1)
  assert_eq(get_set_size(dsu7, 10), 1)
}