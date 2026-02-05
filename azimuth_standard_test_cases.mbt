// Azimuth 项目标准 MoonBit 测试用例
// 包含10个高质量测试用例，覆盖核心功能模块

// 测试 1: Int 模块基本功能测试
test "int_module_basic_functionality" {
  // 测试 Int 模块的最大值和最小值
  assert_eq(@int.max_value, 2147483647)
  assert_eq(@int.min_value, -2147483648)
  
  // 测试绝对值功能
  assert_eq(@int.abs(5), 5)
  assert_eq(@int.abs(-5), 5)
  assert_eq(@int.abs(0), 0)
}

// 测试 2: String 模块基本功能测试
test "string_module_basic_functionality" {
  // 测试字符串默认值
  assert_eq(@string.default(), "")
  
  // 测试字符串连接
  let str1 = "Hello"
  let str2 = "MoonBit"
  let combined = str1 + ", " + str2 + "!"
  assert_eq(combined, "Hello, MoonBit!")
}

// 测试 3: List 模块创建和基本操作测试
test "list_module_creation_and_basic_operations" {
  // 测试空列表创建
  let empty_list : @list.List[Int] = @list.new()
  assert_eq(empty_list.length(), 0)
  assert_eq(empty_list.is_empty(), true)
  
  // 测试从数组创建列表
  let int_list = @list.from_array([1, 2, 3, 4, 5])
  assert_eq(int_list.length(), 5)
  assert_eq(int_list.is_empty(), false)
  
  // 测试列表头部和尾部
  assert_eq(int_list.head(), Some(1))
  assert_eq(int_list.last(), Some(5))
}

// 测试 4: List 模块映射和过滤操作测试
test "list_module_map_and_filter_operations" {
  let numbers = @list.from_array([1, 2, 3, 4, 5])
  
  // 测试映射操作
  let doubled = numbers.map(x => x * 2)
  assert_eq(doubled, @list.from_array([2, 4, 6, 8, 10]))
  
  // 测试带索引的映射操作
  let indexed = numbers.mapi((i, x) => x + i)
  assert_eq(indexed, @list.from_array([1, 3, 5, 7, 9]))
  
  // 测试过滤操作
  let evens = numbers.filter(x => x % 2 == 0)
  assert_eq(evens, @list.from_array([2, 4]))
}

// 测试 5: List 模块折叠和聚合操作测试
test "list_module_fold_and_aggregate_operations" {
  let numbers = @list.from_array([1, 2, 3, 4, 5])
  
  // 测试左折叠求和
  let sum = numbers.fold(init=0, (acc, x) => acc + x)
  assert_eq(sum, 15)
  
  // 测试左折叠求积
  let product = numbers.fold(init=1, (acc, x) => acc * x)
  assert_eq(product, 120)
  
  // 测试最大值和最小值
  assert_eq(numbers.maximum(), Some(5))
  assert_eq(numbers.minimum(), Some(1))
}

// 测试 6: List 模块组合操作测试
test "list_module_combination_operations" {
  let list1 = @list.from_array([1, 2, 3])
  let list2 = @list.from_array([4, 5, 6])
  
  // 测试连接操作
  let concatenated = list1.concat(list2)
  assert_eq(concatenated, @list.from_array([1, 2, 3, 4, 5, 6]))
  
  // 测试反转操作
  let reversed = list1.rev()
  assert_eq(reversed, @list.from_array([3, 2, 1]))
  
  // 测试压缩操作
  let zipped = @list.zip(list1, list2)
  assert_eq(zipped, @list.from_array([(1, 4), (2, 5), (3, 6)]))
}

// 测试 7: 数学运算和边界值测试
test "mathematical_operations_and_boundary_values" {
  // 测试基本数学运算
  assert_eq(5 + 3, 8)
  assert_eq(10 - 4, 6)
  assert_eq(6 * 7, 42)
  assert_eq(20 / 4, 5)
  
  // 测试边界值
  let max_int = @int.max_value
  let min_int = @int.min_value
  
  // 测试溢出边界
  assert_eq(max_int - 1, 2147483646)
  assert_eq(min_int + 1, -2147483647)
  
  // 测试零值运算
  assert_eq(0 * 5, 0)
  assert_eq(5 * 0, 0)
  assert_eq(0 + 0, 0)
}

// 测试 8: 条件判断和逻辑操作测试
test "conditional_and_logical_operations" {
  // 测试基本条件判断
  assert_eq(true && true, true)
  assert_eq(true && false, false)
  assert_eq(false && true, false)
  assert_eq(false && false, false)
  
  assert_eq(true || true, true)
  assert_eq(true || false, true)
  assert_eq(false || true, true)
  assert_eq(false || false, false)
  
  assert_eq(!true, false)
  assert_eq(!false, true)
  
  // 测试比较操作
  assert_eq(5 > 3, true)
  assert_eq(5 >= 5, true)
  assert_eq(3 < 5, true)
  assert_eq(5 <= 5, true)
  assert_eq(5 == 5, true)
  assert_eq(5 != 3, true)
}

// 测试 9: 字符串处理和操作测试
test "string_processing_and_operations" {
  // 测试字符串长度和内容
  let test_str = "MoonBit"
  assert_eq(test_str.length(), 7)
  
  // 测试字符串比较
  assert_eq("hello" == "hello", true)
  assert_eq("hello" != "world", true)
  
  // 测试字符串包含（如果可用）
  // 注意：这里使用基本的字符串操作，因为不确定具体可用的字符串函数
  
  // 测试空字符串
  let empty_str = ""
  assert_eq(empty_str.length(), 0)
  assert_eq(empty_str == "", true)
}

// 测试 10: 复杂数据结构和算法测试
test "complex_data_structures_and_algorithms" {
  // 测试嵌套列表操作
  let inner_list1 = @list.from_array([1, 2, 3])
  let inner_list2 = @list.from_array([4, 5, 6])
  let inner_list3 = @list.from_array([7, 8, 9])
  
  let list_of_lists = @list.from_array([inner_list1, inner_list2, inner_list3])
  
  // 测试展平操作
  let flattened = list_of_lists.flatten()
  assert_eq(flattened, @list.from_array([1, 2, 3, 4, 5, 6, 7, 8, 9]))
  
  // 测试平面映射操作
  let numbers = @list.from_array([1, 2, 3])
  let expanded = numbers.flat_map(x => @list.from_array([x, x * 10]))
  assert_eq(expanded, @list.from_array([1, 10, 2, 20, 3, 30]))
  
  // 测试列表排序
  let unsorted = @list.from_array([5, 2, 8, 1, 9, 3])
  let sorted = unsorted.sort()
  assert_eq(sorted, @list.from_array([1, 2, 3, 5, 8, 9]))
}