/*
 * Sonar Ada Plugin
 *  Copyright (C) 2012-2013, AdaCore
 */
package org.sonar.plugins.ada.utils;

/**
 * Implementation of a Pair data structure
 */
public class Pair<L, R> {

    private L left;
    private R right;

    //<editor-fold desc="Getters & setters" defaultstate="Collapsed">

    public L getLeft() {
        return left;
    }

    public void setLeft(L left) {
        this.left = left;
    }

    public R getRight() {
        return right;
    }

    public void setRight(R right) {
        this.right = right;
    }
//</editor-fold>

    public Pair(L left, R right) {
        this.left = left;
        this.right = right;
    }

    private boolean equals(Object x, Object y) {
        return (x == null && y == null) || (x != null && x.equals(y));
    }

    @Override
    public boolean equals(Object other) {
        if (this == other) {
            return true;
        }

        return other instanceof Pair
                && equals(left, ((Pair) other).left)
                && equals(right, ((Pair) other).right);
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result + ((left == null) ? 0 : left.hashCode());
        result = prime * result + ((right == null) ? 0 : right.hashCode());
        return result;
    }

    @Override
    public String toString() {
        return "Pair: " + left + "," + right;
    }
}