module Control.If where

if' True x _ = x
if' _    _ y = y
