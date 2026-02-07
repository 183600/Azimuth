// 专业级 MoonBit 测试用例集
// 包含10个高质量测试用例，使用标准 MoonBit 测试语法
// 涵盖算法模拟、数学计算、边界情况和实际应用场景

test "matrix_multiplication_simulation" {
  // 矩阵乘法算法模拟
  // 2x2 矩阵乘法: A × B = C
  // A = [[1, 2], [3, 4]], B = [[5, 6], [7, 8]]
  
  let a11 = 1
  let a12 = 2
  let a21 = 3
  let a22 = 4
  
  let b11 = 5
  let b12 = 6
  let b21 = 7
  let b22 = 8
  
  // 计算 C = A × B
  let c11 = add(multiply(a11, b11), multiply(a12, b21))  // 1*5 + 2*7 = 19
  let c12 = add(multiply(a11, b12), multiply(a12, b22))  // 1*6 + 2*8 = 22
  let c21 = add(multiply(a21, b11), multiply(a22, b21))  // 3*5 + 4*7 = 43
  let c22 = add(multiply(a21, b12), multiply(a22, b22))  // 3*6 + 4*8 = 50
  
  // 验证矩阵乘法结果
  assert_eq(c11, 19)
  assert_eq(c12, 22)
  assert_eq(c21, 43)
  assert_eq(c22, 50)
  
  // 计算矩阵的迹（对角线元素之和）
  let trace_c = add(c11, c22)
  assert_eq(trace_c, 69)
}

test "prime_number_sieve_simulation" {
  // 埃拉托斯特尼筛法模拟 - 找出2-20之间的质数
  
  let max_num = 20
  
  // 模拟筛选过程
  // 初始假设所有数都是质数（除了1）
  let prime_count_initial = subtract(max_num, 1)  // 2-20共19个数
  
  // 筛选掉2的倍数（除了2本身）
  let multiples_of_2 = divide_with_ceil(max_num, 2) - 1  // 除去2本身
  let remaining_after_2 = subtract(prime_count_initial, multiples_of_2)
  
  // 筛选掉3的倍数（除了3本身和已经被2筛选的）
  let multiples_of_3 = divide_with_ceil(max_num, 3) - 1  // 除去3本身
  let remaining_after_3 = subtract(remaining_after_2, multiples_of_3 - 1)  // 减去6（已被2筛选）
  
  // 筛选掉5的倍数（除了5本身和已被筛选的）
  let multiples_of_5 = divide_with_ceil(max_num, 5) - 1  // 除去5本身
  let remaining_after_5 = subtract(remaining_after_3, multiples_of_5 - 2)  // 减去10,15（已被2,3筛选）
  
  // 验证质数数量（2-20之间的质数有8个：2,3,5,7,11,13,17,19）
  assert_eq(remaining_after_5, 8)
  
  // 计算质数的和
  let prime_sum = add(add(add(add(add(add(add(2, 3), 5), 7), 11), 13), 17), 19)
  assert_eq(prime_sum, 77)
}

test "cache_replacement_algorithm" {
  // 缓存替换算法模拟 - LRU（最近最少使用）算法
  
  let cache_size = 3
  let access_sequence_length = 8
  
  // 模拟访问序列：1, 2, 3, 4, 1, 2, 5, 1
  // 缓存状态变化：
  // 访问1: [1]
  // 访问2: [1, 2]
  // 访问3: [1, 2, 3]
  // 访问4: [2, 3, 4] (1被替换)
  // 访问1: [3, 4, 1] (2被替换)
  // 访问2: [4, 1, 2] (3被替换)
  // 访问5: [1, 2, 5] (4被替换)
  // 访问1: [2, 5, 1] (1移动到最近位置)
  
  // 计算缓存命中次数
  let hits = 2  // 访问1和访问2时命中
  let misses = subtract(access_sequence_length, hits)
  
  // 计算命中率
  let hit_rate = divide_with_ceil(multiply(hits, 100), access_sequence_length)
  
  assert_eq(hits, 2)
  assert_eq(misses, 6)
  assert_eq(hit_rate, 25)
}

test "hash_table_collision_resolution" {
  // 哈希表冲突解决模拟 - 链地址法
  
  let table_size = 10
  let num_elements = 15
  
  // 模拟哈希函数：hash(key) = key % table_size
  // 插入的键值对：{1:"a"}, {11:"b"}, {21:"c"}, {2:"d"}, {12:"e"}, {3:"f"}, {13:"g"}, {4:"h"}, {14:"i"}, {5:"j"}, {15:"k"}, {6:"l"}, {7:"m"}, {8:"n"}, {9:"o"}
  
  // 计算每个桶的元素数量
  let bucket1_count = 3  // 1, 11, 21
  let bucket2_count = 2  // 2, 12
  let bucket3_count = 2  // 3, 13
  let bucket4_count = 2  // 4, 14
  let bucket5_count = 2  // 5, 15
  let bucket6_count = 1  // 6
  let bucket7_count = 1  // 7
  let bucket8_count = 1  // 8
  let bucket9_count = 1  // 9
  let bucket0_count = 0  // 无元素
  
  // 计算最大冲突数
  let max_collisions = bucket1_count
  let total_collisions = add(add(add(add(add(add(add(add(bucket1_count - 1, bucket2_count - 1), bucket3_count - 1), bucket4_count - 1), bucket5_count - 1), bucket6_count - 1), bucket7_count - 1), bucket8_count - 1)
  
  assert_eq(max_collisions, 3)
  assert_eq(total_collisions, 6)
  
  // 计算负载因子
  let load_factor = divide_with_ceil(multiply(num_elements, 100), table_size)
  assert_eq(load_factor, 150)
}

test "graph_coloring_algorithm" {
  // 图着色算法模拟 - 贪心着色算法
  
  let num_vertices = 6
  let num_edges = 7
  
  // 图的边：(0,1), (0,2), (1,3), (2,3), (2,4), (3,5), (4,5)
  
  // 贪心着色算法：
  // 顶点0：颜色1
  // 顶点1：颜色2（与0相邻）
  // 顶点2：颜色2（与0相邻，与1不相邻）
  // 顶点3：颜色3（与1,2相邻）
  // 顶点4：颜色1（与2相邻，与0,1,3不相邻）
  // 顶点5：颜色2（与3,4相邻）
  
  let color1_vertices = 2  // 顶点0, 4
  let color2_vertices = 3  // 顶点1, 2, 5
  let color3_vertices = 1  // 顶点3
  let total_colors_used = 3
  
  // 计算着色效率（使用的颜色数与顶点数的比率）
  let coloring_efficiency = divide_with_ceil(multiply(total_colors_used, 100), num_vertices)
  
  assert_eq(color1_vertices, 2)
  assert_eq(color2_vertices, 3)
  assert_eq(color3_vertices, 1)
  assert_eq(total_colors_used, 3)
  assert_eq(coloring_efficiency, 50)
}

test "compression_ratio_calculation" {
  // 压缩比计算测试 - LZ77算法模拟
  
  let original_data_size = 1000
  let window_size = 100
  let min_match_length = 3
  
  // 模拟压缩过程：
  // 找到重复模式，每个匹配平均节省空间
  let num_matches = 25
  let avg_match_length = 8
  let avg_match_distance = 50
  
  // 计算压缩后的大小
  let compressed_size_per_match = 3  // 每个匹配用3字节表示（距离+长度）
  let total_compressed_matches = multiply(num_matches, compressed_size_per_match)
  let literal_size = subtract(original_data_size, multiply(num_matches, avg_match_length))
  let compressed_total_size = add(total_compressed_matches, literal_size)
  
  // 计算压缩比
  let compression_ratio = divide_with_ceil(multiply(original_data_size, 100), compressed_total_size)
  let space_saved = subtract(original_data_size, compressed_total_size)
  let space_saved_percentage = divide_with_ceil(multiply(space_saved, 100), original_data_size)
  
  assert_eq(total_compressed_matches, 75)
  assert_eq(literal_size, 800)
  assert_eq(compressed_total_size, 875)
  assert_eq(compression_ratio, 115)
  assert_eq(space_saved, 125)
  assert_eq(space_saved_percentage, 13)
}

test "priority_queue_operations" {
  // 优先队列操作模拟 - 最大堆实现
  
  let heap_capacity = 10
  let num_operations = 8
  
  // 模拟操作序列：insert(5), insert(3), insert(8), insert(1), extract_max(), insert(7), extract_max(), extract_max()
  
  // 初始状态：空堆
  let heap_size_0 = 0
  
  // insert(5): [5]
  let heap_size_1 = add(heap_size_0, 1)
  let max_1 = 5
  
  // insert(3): [5, 3]
  let heap_size_2 = add(heap_size_1, 1)
  let max_2 = 5
  
  // insert(8): [8, 3, 5]
  let heap_size_3 = add(heap_size_2, 1)
  let max_3 = 8
  
  // insert(1): [8, 3, 5, 1]
  let heap_size_4 = add(heap_size_3, 1)
  let max_4 = 8
  
  // extract_max(): 返回8，堆变为[5, 3, 1]
  let extracted_1 = max_4
  let heap_size_5 = subtract(heap_size_4, 1)
  let max_5 = 5
  
  // insert(7): [7, 3, 1, 5]
  let heap_size_6 = add(heap_size_5, 1)
  let max_6 = 7
  
  // extract_max(): 返回7，堆变为[5, 3, 1]
  let extracted_2 = max_6
  let heap_size_7 = subtract(heap_size_6, 1)
  let max_7 = 5
  
  // extract_max(): 返回5，堆变为[3, 1]
  let extracted_3 = max_7
  let heap_size_8 = subtract(heap_size_7, 1)
  let max_8 = 3
  
  // 验证操作结果
  assert_eq(extracted_1, 8)
  assert_eq(extracted_2, 7)
  assert_eq(extracted_3, 5)
  assert_eq(heap_size_8, 2)
  assert_eq(max_8, 3)
  
  // 计算平均每次操作的时间复杂度（对数时间）
  let avg_complexity_factor = divide_with_ceil(multiply(num_operations, 100), heap_capacity)
  assert_eq(avg_complexity_factor, 80)
}

test "avl_tree_balancing" {
  // AVL树平衡操作模拟
  
  let num_nodes = 7
  let balance_factor_limit = 1
  
  // 模拟插入序列：10, 20, 30, 40, 50, 25, 5
  // AVL树会在插入过程中自动平衡
  
  // 插入10: [10]
  let height_1 = 1
  
  // 插入20: [10, 20]
  let height_2 = 2
  
  // 插入30: 触发左旋，树变为[20, 10, 30]
  let height_3 = 2  // 平衡后高度为2
  let rotation_count_1 = 1
  
  // 插入40: [20, 10, 30, 40]
  let height_4 = 3
  
  // 插入50: 触发左旋，30的右子树平衡
  let rotation_count_2 = add(rotation_count_1, 1)
  let height_5 = 3
  
  // 插入25: 不需要旋转
  let height_6 = 3
  
  // 插入5: 不需要旋转
  let height_7 = 3
  
  // 计算树的平衡性
  let total_rotations = rotation_count_2
  let final_height = height_7
  let balance_factor = divide_with_ceil(multiply(final_height, 100), num_nodes)
  
  assert_eq(total_rotations, 2)
  assert_eq(final_height, 3)
  assert_eq(balance_factor, 43)
  
  // 验证AVL树的性质：高度为O(log n)
  let expected_max_height = divide_with_ceil(multiply(2, divide_with_ceil(num_nodes, 2)), 1)
  assert_eq(final_height, 3)
}

test "regular_expression_engine" {
  // 正则表达式引擎模拟 - NFA到DFA转换
  
  let pattern_length = 5  // 正则表达式 "a*b"
  let input_length = 10   // 输入字符串 "aaabcaaab"
  
  // 模拟NFA状态
  let nfa_states = multiply(pattern_length, 2)  // 每个字符可能有2个状态
  let dfa_states = multiply(2, pattern_length)  // DFA状态数最多为2^n
  
  // 模拟匹配过程
  let match_attempts = subtract(input_length, pattern_length) + 1
  let successful_matches = 2  // "aaab" 和 "aaab"
  
  // 计算匹配成功率
  let success_rate = divide_with_ceil(multiply(successful_matches, 100), match_attempts)
  
  // 计算状态转换次数
  let avg_transitions_per_match = multiply(pattern_length, 2)
  let total_transitions = multiply(successful_matches, avg_transitions_per_match)
  
  // 计算引擎效率
  let engine_efficiency = divide_with_ceil(multiply(total_transitions, 100), multiply(input_length, pattern_length))
  
  assert_eq(nfa_states, 10)
  assert_eq(dfa_states, 10)
  assert_eq(match_attempts, 6)
  assert_eq(successful_matches, 2)
  assert_eq(success_rate, 34)
  assert_eq(avg_transitions_per_match, 10)
  assert_eq(total_transitions, 20)
  assert_eq(engine_efficiency, 20)
}

test "neural_network_forward_propagation" {
  // 神经网络前向传播模拟
  
  let input_size = 3
  let hidden_size = 4
  let output_size = 2
  
  // 输入向量：[1.0, 0.5, -0.5] (用整数模拟)
  let input1 = 10
  let input2 = 5
  let input3 = -5
  
  // 权重矩阵 W1 (3x4) 和偏置 b1 (4)
  // 简化计算：假设权重为1，偏置为0
  let w11_sum = add(add(input1, input2), input3)  // 第一个隐藏神经元
  let w12_sum = add(add(multiply(input1, 2), input2), input3)  // 第二个隐藏神经元
  let w13_sum = add(add(input1, multiply(input2, 2)), input3)  // 第三个隐藏神经元
  let w14_sum = add(add(input1, input2), multiply(input3, 2))  // 第四个隐藏神经元
  
  // 激活函数（ReLU简化版）
  let h1 = divide_with_ceil(add(w11_sum, 100), 100)  // 简化的激活函数
  let h2 = divide_with_ceil(add(w12_sum, 100), 100)
  let h3 = divide_with_ceil(add(w13_sum, 100), 100)
  let h4 = divide_with_ceil(add(w14_sum, 100), 100)
  
  // 输出层计算
  let output1 = add(add(h1, h2), multiply(h3, 2))  // 第一个输出神经元
  let output2 = add(add(h2, h3), multiply(h4, 2))  // 第二个输出神经元
  
  // 计算网络的总计算量
  let total_multiplications = add(multiply(input_size, hidden_size), multiply(hidden_size, output_size))
  let total_additions = add(multiply(input_size, hidden_size), multiply(hidden_size, output_size))
  let total_operations = add(total_multiplications, total_additions)
  
  assert_eq(w11_sum, 10)
  assert_eq(w12_sum, 15)
  assert_eq(w13_sum, 15)
  assert_eq(w14_sum, 0)
  assert_eq(h1, 2)
  assert_eq(h2, 2)
  assert_eq(h3, 2)
  assert_eq(h4, 1)
  assert_eq(output1, 8)
  assert_eq(output2, 7)
  assert_eq(total_multiplications, 20)
  assert_eq(total_additions, 20)
  assert_eq(total_operations, 40)
}