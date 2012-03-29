package org.siemac.metamac.statistical.operations.core.domain;

import javax.persistence.Entity;
import javax.persistence.Table;
import javax.persistence.UniqueConstraint;

/**
 * Statistical instance
 */
@Entity
@Table(name = "TBL_INSTANCES", uniqueConstraints = {@UniqueConstraint(columnNames = {"CODE"})})
public class Instance extends InstanceBase {

    private static final long serialVersionUID = 1L;

    public Instance() {
    }
}
