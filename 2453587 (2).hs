-- gökçesu terme
-- 2453587
-- I read and accept the submission rules and the extra rules specified
--in each question. This is my own work that is done by myself only.

-- part 1 University Organisation Tree 
data Tree = Leaf String Int | Node String Int [Tree] deriving (Show)

constructTree :: [(String, Int)] -> [Int] -> Tree
constructTree departments childrenCounts =
  let (_, trees) = foldr (\(dept, childCount) (remainingDepts, x) ->
                              let (deptName, empCount) = dept
                              in if childCount == 0 then (remainingDepts, Leaf deptName empCount : x)
                                 else (remainingDepts, Node deptName empCount (take childCount x) : drop childCount x))
                          (departments, []) (zipWith (\a b -> (a, b)) departments childrenCounts)
  in head trees


unitree :: [(String, Int)] -> [Int] -> Tree
unitree departments counts = constructTree departments counts

-- part 2 question
-- sectionsize function
-- this function is resbonsible for counting the employee numbers of the given nodes all children.
count_children :: String -> Tree -> Int
count_children node_name tree = sumChildren tree where
    sumChildren :: Tree -> Int
    sumChildren (Leaf _ _) = 0  -- Leaves do not have children
    sumChildren (Node name _ children)
      | name == node_name = sum (map total_count children) 
      | otherwise = sum (map sumChildren children)  

    -- Helper function to recursively sum the employee numbers
    total_count (Leaf _ count) = count
    total_count (Node _ count children) = count + sum (map total_count children)

-- this function will only be resbonsible for the employee count of the given node name
parent_count :: String -> Tree -> Int
parent_count node_name (Leaf name empCount)
  | node_name == name = empCount
  | otherwise = 0
parent_count node_name (Node name empCount children)
  | node_name == name = empCount
  | otherwise = sum (map (parent_count node_name) children)

-- sum all the children count and the parent count to find the sectionsize
sectionsize tree node_name = parent_count node_name tree + count_children node_name tree

-- managingentity function implementation
managingentity :: Tree -> String -> String
managingentity tree dept_name = findParent "" tree
  where
    findParent parent (Leaf name _) 
      | name == dept_name = parent
      | otherwise = "Not Found"
    findParent parent (Node name _ children) 
      | any (isNodeName dept_name) children = name
      | otherwise = foldr (\child x -> if x == "Not Found" then findParent name child else x) "Not Found" children

    isNodeName tmp (Leaf name _) = tmp == name
    isNodeName tmp (Node name _ _) = tmp == name

-- managelist function implementation
managelist :: Tree -> String -> [String]
managelist tree in_dept = reverse (findParents [] "" tree) 
  where
    findParents x currentName (Leaf name _)
      | name == in_dept = currentName : x
      | otherwise = []
    findParents x currentName (Node name _ children)
      | name == in_dept = currentName : x
      | otherwise = foldr (\child y -> let parents = findParents (if not (null currentName) then currentName : x else x) name child
                                         in if not (null parents) then parents else y) [] children
    isNodeName tmp (Leaf name _) = tmp == name
    isNodeName tmp (Node name _ _) = tmp == name

