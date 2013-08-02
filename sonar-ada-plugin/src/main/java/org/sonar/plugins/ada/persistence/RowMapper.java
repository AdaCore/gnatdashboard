/*
 * Sonar Ada Plugin
 *  Copyright (C) 2012-2013, AdaCore
 */
package org.sonar.plugins.ada.persistence;

/**
 * Row mapper interface
 */
public interface RowMapper<T> {

  T mapRow(java.sql.ResultSet resultSet) throws java.sql.SQLException;
}
