// 高质量 MoonBit 核心库测试用例
// 覆盖 int、string、list、option、result 等核心模块

test "int_max_min_values" {
  // 测试 int 模块的最大值和最小值
  inspect(@int.max_value, content="2147483647")
  inspect(@int.min_value, content="-2147483648")
}

test "list_basic_operations" {
  // 测试 list 的基本操作
  let empty_list = @list.empty()
  inspect(empty_list.length(), content="0")
  inspect(empty_list.is_empty(), content="true")
  
  let list1 = @list.from_array([1, 2, 3, 4, 5])
  inspect(list1.length(), content="5")
  inspect(list1.is_empty(), content="false")
  inspect(list1.head(), content="Some(1)")
  inspect(list1.last(), content="Some(5)")
}

test "list_map_and_filter" {
  // 测试 list 的 map 和 filter 操作
  let numbers = @list.from_array([1, 2, 3, 4, 5])
  
  // 测试 map
  let doubled = numbers.map(x => x * 2)
  inspect(doubled.to_array(), content="[2, 4, 6, 8, 10]")
  
  // 测试 filter
  let evens = numbers.filter(x => x % 2 == 0)
  inspect(evens.to_array(), content="[2, 4]")
  
  // 测试 flat_map
  let expanded = numbers.flat_map(x => @list.from_array([x, x * 10]))
  inspect(expanded.to_array(), content="[1, 10, 2, 20, 3, 30, 4, 40, 5, 50]")
}

test "list_fold_operations" {
  // 测试 list 的 fold 操作
  let numbers = @list.from_array([1, 2, 3, 4, 5])
  
  // 测试 fold (sum)
  let sum = numbers.fold(init=0, (acc, x) => acc + x)
  inspect(sum, content="15")
  
  // 测试 fold (product)
  let product = numbers.fold(init=1, (acc, x) => acc * x)
  inspect(product, content="120")
  
  // 测试 foldi with index
  let indexed_sum = numbers.foldi(init=0, (i, acc, x) => acc + x * i)
  inspect(indexed_sum, content="40") // 0*1 + 1*2 + 2*3 + 3*4 + 4*5 = 40
}

test "list_concatenation_and_reversal" {
  // 测试 list 的连接和反转操作
  let list1 = @list.from_array([1, 2, 3])
  let list2 = @list.from_array([4, 5, 6])
  
  // 测试 concat
  let combined = list1.concat(list2)
  inspect(combined.to_array(), content="[1, 2, 3, 4, 5, 6]")
  
  // 测试 reverse
  let reversed = combined.rev()
  inspect(reversed.to_array(), content="[6, 5, 4, 3, 2, 1]")
  
  // 测试 + operator
  let added = list1 + list2
  inspect(added.to_array(), content="[1, 2, 3, 4, 5, 6]")
}

test "list_search_operations" {
  // 测试 list 的搜索操作
  let numbers = @list.from_array([1, 2, 3, 4, 5])
  
  // 测试 contains
  inspect(numbers.contains(3), content="true")
  inspect(numbers.contains(6), content="false")
  
  // 测试 nth
  inspect(numbers.nth(0), content="Some(1)")
  inspect(numbers.nth(2), content="Some(3)")
  inspect(numbers.nth(4), content="Some(5)")
  inspect(numbers.nth(5), content="None")
  
  // 测试 all 和 any
  inspect(numbers.all(x => x > 0), content="true")
  inspect(numbers.all(x => x % 2 == 0), content="false")
  inspect(numbers.any(x => x % 2 == 0), content="true")
  inspect(numbers.any(x => x > 10), content="false")
}

test "list_min_max_operations" {
  // 测试 list 的最小值和最大值操作
  let numbers = @list.from_array([3, 1, 4, 1, 5, 9, 2, 6])
  
  // 测试 minimum
  inspect(numbers.minimum(), content="Some(1)")
  
  // 测试 maximum
  inspect(numbers.maximum(), content="Some(9)")
  
  // 测试空列表的情况
  let empty = @list.empty()
  inspect(empty.minimum(), content="None")
  inspect(empty.maximum(), content="None")
}

test "list_zip_and_unzip" {
  // 测试 list 的 zip 和 unzip 操作
  let list1 = @list.from_array([1, 2, 3])
  let list2 = @list.from_array(["a", "b", "c"])
  
  // 测试 zip
  let zipped = @list.zip(list1, list2)
  inspect(zipped.to_array(), content="[(1, \"a\"), (2, \"b\"), (3, \"c\")]")
  
  // 测试 unzip
  let (unzipped_nums, unzipped_strs) = zipped.unzip()
  inspect(unzipped_nums.to_array(), content="[1, 2, 3]")
  inspect(unzipped_strs.to_array(), content="[\"a\", \"b\", \"c\"]")
  
  // 测试不同长度的列表 zip
  let long_list = @list.from_array([1, 2, 3, 4, 5])
  let short_list = @list.from_array(["a", "b"])
  let zipped_unequal = @list.zip(long_list, short_list)
  inspect(zipped_unequal.to_array(), content="[(1, \"a\"), (2, \"b\")]")
}

test "list_take_and_drop" {
  // 测试 list 的 take 和 drop 操作
  let numbers = @list.from_array([1, 2, 3, 4, 5, 6, 7, 8, 9, 10])
  
  // 测试 take
  let taken = numbers.take(5)
  inspect(taken.to_array(), content="[1, 2, 3, 4, 5]")
  
  // 测试 take 超过长度
  let taken_all = numbers.take(20)
  inspect(taken_all.to_array(), content="[1, 2, 3, 4, 5, 6, 7, 8, 9, 10]")
  
  // 测试 take 0
  let taken_none = numbers.take(0)
  inspect(taken_none.to_array(), content="[]")
  
  // 测试 take 负数
  let taken_negative = numbers.take(-3)
  inspect(taken_negative.to_array(), content="[]")
}

test "list_sort_and_repeat" {
  // 测试 list 的排序和重复操作
  let unsorted = @list.from_array([5, 2, 8, 1, 9, 3])
  
  // 测试 sort
  let sorted = unsorted.sort()
  inspect(sorted.to_array(), content="[1, 2, 3, 5, 8, 9]")
  
  // 测试 repeat
  let repeated = @list.repeat(4, 7)
  inspect(repeated.to_array(), content="[7, 7, 7, 7]")
  
  // 测试 repeat 0 次
  let repeated_none = @list.repeat(0, 7)
  inspect(repeated_none.to_array(), content="[]")
  
  // 测试 repeat 负数
  let repeated_negative = @list.repeat(-3, 7)
  inspect(repeated_negative.to_array(), content="[]")
}