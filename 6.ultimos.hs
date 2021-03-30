--ultimos :: Int → [a] → [a]
ultimos n = reverse . take n . reverse 
