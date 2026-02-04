// 标准 MoonBit 测试用例
// 测试核心库功能

test "integer_absolute_value" {
  // 测试整数绝对值功能
  assert_eq(5, @int.abs(5))
  assert_eq(5, @int.abs(-5))
  assert_eq(0, @int.abs(0))
  assert_eq(2147483647, @int.abs(2147483647))
  assert_eq(2147483648, @int.abs(-2147483648))
}

test "integer_max_min_values" {
  // 测试整数最大值和最小值
  assert_eq(2147483647, @int.max_value)
  assert_eq(-2147483648, @int.min_value)
  
  // 验证边界值操作
  let max_plus_one = @int.add(@int.max_value, 1)
  let min_minus_one = @int.add(@int.min_value, -1)
  
  assert_eq(-2147483648, max_plus_one)  // 溢出环绕
  assert_eq(2147483647, min_minus_one)  // 溢出环绕
}

test "string_default_functionality" {
  // 测试字符串默认功能
  assert_eq_string("", @string.default())
  
  // 测试字符串连接
  let hello = "Hello"
  let world = "World"
  let combined = hello + ", " + world + "!"
  assert_eq_string("Hello, World!", combined)
}

test "list_basic_operations" {
  // 测试列表基本操作
  let empty_list : @list.List[Int] = @list.empty()
  assert_eq(true, empty_list.is_empty())
  assert_eq(0, empty_list.length())
  
  // 测试列表构建
  let list1 = @list.from_array([1, 2, 3, 4, 5])
  assert_eq(false, list1.is_empty())
  assert_eq(5, list1.length())
  assert_eq(Some(1), list1.head())
  assert_eq(Some(5), list1.last())
}

test "list_map_operations" {
  // 测试列表映射操作
  let numbers = @list.from_array([1, 2, 3, 4, 5])
  
  // 测试 map
  let doubled = numbers.map(x => x * 2)
  assert_eq(@list.from_array([2, 4, 6, 8, 10]), doubled)
  
  // 测试 filter
  let evens = numbers.filter(x => x % 2 == 0)
  assert_eq(@list.from_array([2, 4]), evens)
  
  // 测试 fold
  let sum = numbers.fold(init=0, (acc, x) => acc + x)
  assert_eq(15, sum)
}

test "list_concatenation_operations" {
  // 测试列表连接操作
  let list1 = @list.from_array([1, 2, 3])
  let list2 = @list.from_array([4, 5, 6])
  
  // 测试 concat
  let combined = list1.concat(list2)
  assert_eq(@list.from_array([1, 2, 3, 4, 5, 6]), combined)
  
  // 测试 reverse
  let reversed = combined.rev()
  assert_eq(@list.from_array([6, 5, 4, 3, 2, 1]), reversed)
  
  // 测试 + 操作符
  let added = list1 + list2
  assert_eq(combined, added)
}

test "list_search_operations" {
  // 测试列表搜索操作
  let numbers = @list.from_array([10, 20, 30, 40, 50])
  
  // 测试 contains
  assert_eq(true, numbers.contains(30))
  assert_eq(false, numbers.contains(25))
  
  // 测试 nth
  assert_eq(Some(30), numbers.nth(2))
  assert_eq(None, numbers.nth(10))
  
  // 测试 all 和 any
  assert_eq(true, numbers.all(x => x >= 10))
  assert_eq(true, numbers.any(x => x > 45))
  assert_eq(false, numbers.any(x => x > 100))
}

test "list_min_max_operations" {
  // 测试列表最小最大值操作
  let numbers = @list.from_array([5, 2, 8, 1, 9, 3])
  
  // 测试 maximum 和 minimum
  assert_eq(Some(9), numbers.maximum())
  assert_eq(Some(1), numbers.minimum())
  
  // 测试空列表
  let empty : @list.List[Int] = @list.empty()
  assert_eq(None, empty.maximum())
  assert_eq(None, empty.minimum())
  
  // 测试单元素列表
  let single = @list.from_array([42])
  assert_eq(Some(42), single.maximum())
  assert_eq(Some(42), single.minimum())
}

test "list_zip_operations" {
  // 测试列表配对操作
  let list1 = @list.from_array([1, 2, 3])
  let list2 = @list.from_array(["a", "b", "c"])
  
  // 测试 zip
  let zipped = @list.zip(list1, list2)
  assert_eq(@list.from_array([(1, "a"), (2, "b"), (3, "c")]), zipped)
  
  // 测试不同长度的列表
  let short1 = @list.from_array([1, 2])
  let long2 = @list.from_array(["a", "b", "c", "d"])
  let zipped_diff = @list.zip(short1, long2)
  assert_eq(@list.from_array([(1, "a"), (2, "b")]), zipped_diff)
  
  // 测试 unzip
  let (unzipped_nums, unzipped_strs) = zipped.unzip()
  assert_eq(list1, unzipped_nums)
  assert_eq(list2, unzipped_strs)
}

test "list_repeat_and_take" {
  // 测试列表重复和取元素操作
  // 测试 repeat
  let repeated = @list.repeat(4, 7)
  assert_eq(@list.from_array([7, 7, 7, 7]), repeated)
  
  // 测试 take
  let long_list = @list.from_array([1, 2, 3, 4, 5, 6, 7, 8, 9, 10])
  let taken = long_list.take(5)
  assert_eq(@list.from_array([1, 2, 3, 4, 5]), taken)
  
  // 测试 take 超过列表长度
  let taken_all = long_list.take(20)
  assert_eq(long_list, taken_all)
  
  // 测试 take 0
  let taken_none = long_list.take(0)
  assert_eq(@list.empty(), taken_none)
}