package org.siemac.metamac.statistical.operations.core.domain;

import javax.persistence.Entity;
import javax.persistence.Table;
import javax.persistence.UniqueConstraint;

/**
 * Semantic grouping of statistical operations
 */
@Entity
@Table(name = "TB_FAMILIES", uniqueConstraints = {@UniqueConstraint(columnNames = {"CODE"})})
public class Family extends FamilyBase {

    private static final long serialVersionUID = 1L;

    public Family() {
    }
}
