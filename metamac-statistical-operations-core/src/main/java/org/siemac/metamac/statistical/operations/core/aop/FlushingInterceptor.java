package org.siemac.metamac.statistical.operations.core.aop;

import javax.persistence.EntityManager;
import javax.persistence.PersistenceContext;

import org.siemac.metamac.core.common.aop.FlushingInterceptorBase;

public class FlushingInterceptor extends FlushingInterceptorBase {

    @Override
    @PersistenceContext(unitName = "StatisticalOperationsEntityManagerFactory")
    protected void setEntityManager(EntityManager entityManager) {
        this.entityManager = entityManager;
    }
}