package org.siemac.metamac.statistical.operations.core.ws.criteria.mapper;

import java.util.List;

import org.fornax.cartridges.sculptor.framework.accessapi.ConditionalCriteria;
import org.fornax.cartridges.sculptor.framework.accessapi.ConditionalCriteriaBuilder;
import org.fornax.cartridges.sculptor.framework.accessapi.ConditionalCriteriaBuilder.CondditionProperty;
import org.fornax.cartridges.sculptor.framework.accessapi.ConditionalCriteriaBuilder.ConditionRoot;
import org.fornax.cartridges.sculptor.framework.domain.PagingParameter;
import org.fornax.cartridges.sculptor.framework.domain.Property;
import org.siemac.metamac.core.common.criteria.MetamacCriteriaOrder.OrderTypeEnum;
import org.siemac.metamac.core.common.criteria.SculptorCriteria;
import org.siemac.metamac.core.common.criteria.SculptorPropertyCriteria;
import org.siemac.metamac.core.common.exception.CommonServiceExceptionType;
import org.siemac.metamac.core.common.exception.MetamacException;
import org.siemac.metamac.schema.common.v1_0.domain.MetamacCriteria;
import org.siemac.metamac.schema.common.v1_0.domain.MetamacCriteriaConjunctionRestriction;
import org.siemac.metamac.schema.common.v1_0.domain.MetamacCriteriaDisjunctionRestriction;
import org.siemac.metamac.schema.common.v1_0.domain.MetamacCriteriaOrder;
import org.siemac.metamac.schema.common.v1_0.domain.MetamacCriteriaPropertyRestriction;
import org.siemac.metamac.schema.common.v1_0.domain.MetamacCriteriaRestriction;

// TODO pasar a librería común cuando se haga el resto de servicios web, y se decida si son SOAP o REST
@SuppressWarnings({"unchecked", "rawtypes"})
public class MetamacCriteriaWebService2SculptorCriteria<T> {

    private Class<T>              entityClass                  = null;

    private Class<? extends Enum> propertyOrderEnumClass       = null;
    private Class<? extends Enum> propertyRestrictionEnumClass = null;

    private CriteriaCallback      callback                     = null;

    private Integer               MAXIMUM_RESULT_SIZE_DEFAULT  = Integer.valueOf(25);
    private Integer               MAXIMUM_RESULT_SIZE_ALLOWED  = Integer.valueOf(1000);

    public MetamacCriteriaWebService2SculptorCriteria(Class<T> entityClass, Class<? extends Enum> propertyOrderEnumClass, Class<? extends Enum> propertyRestrictionEnumClass, CriteriaCallback callback)
            throws MetamacException {

        this.entityClass = entityClass;
        this.propertyOrderEnumClass = propertyOrderEnumClass;
        this.propertyRestrictionEnumClass = propertyRestrictionEnumClass;
        this.callback = callback;
    }

    public SculptorCriteria metamacCriteria2SculptorCriteria(MetamacCriteria metamacCriteria) throws MetamacException {

        if (metamacCriteria == null) {
            metamacCriteria = new MetamacCriteria();
        }

        ConditionRoot<T> criteria = ConditionalCriteriaBuilder.criteriaFor(entityClass);

        // Orders
        if (metamacCriteria.getOrdersBy() == null) {
            Property defaultOrder = callback.retrievePropertyOrderDefault();
            if (defaultOrder != null) {
                criteria.orderBy(defaultOrder).ascending();
            }
        } else {
            for (MetamacCriteriaOrder order : metamacCriteria.getOrdersBy().getOrder()) {
                checkPropertyOrder(order);
                if (order.getType() == null || OrderTypeEnum.ASC.equals(order.getType())) {
                    criteria.orderBy(callback.retrievePropertyOrder(order)).ascending();
                } else {
                    criteria.orderBy(callback.retrievePropertyOrder(order)).descending();
                }
            }
        }

        // Restrictions
        if (metamacCriteria.getRestriction() != null) {
            addRestriction(metamacCriteria.getRestriction(), criteria);
        }

        List<ConditionalCriteria> conditions = criteria.distinctRoot().build();

        // Paging parameter
        Integer startRow = null;
        Integer maximumResultSize = null;
        Boolean countTotalResults = null;
        if (metamacCriteria != null) {
            if (metamacCriteria.getFirstResult() != null) {
                startRow = metamacCriteria.getFirstResult().intValue();
            }
            if (metamacCriteria.getMaxResults() != null) {
                maximumResultSize = metamacCriteria.getMaxResults().intValue();
            }
            countTotalResults = metamacCriteria.getDoCount();
        }
        if (startRow == null || startRow < 0) {
            startRow = Integer.valueOf(0);
        }
        if (maximumResultSize == null) {
            maximumResultSize = MAXIMUM_RESULT_SIZE_DEFAULT;
        }
        if (maximumResultSize > MAXIMUM_RESULT_SIZE_ALLOWED) {
            maximumResultSize = MAXIMUM_RESULT_SIZE_ALLOWED;
        }
        if (countTotalResults == null) {
            countTotalResults = Boolean.FALSE;
        }
        PagingParameter pagingParameter = PagingParameter.rowAccess(startRow, startRow + maximumResultSize, countTotalResults);

        return new SculptorCriteria(conditions, pagingParameter, maximumResultSize);
    }

    private void addRestriction(MetamacCriteriaRestriction metamacCriteriaRestriction, ConditionRoot criteria) throws MetamacException {

        if (metamacCriteriaRestriction instanceof MetamacCriteriaDisjunctionRestriction) {
            addRestriction((MetamacCriteriaDisjunctionRestriction) metamacCriteriaRestriction, criteria);
        } else if (metamacCriteriaRestriction instanceof MetamacCriteriaConjunctionRestriction) {
            addRestriction((MetamacCriteriaConjunctionRestriction) metamacCriteriaRestriction, criteria);
        } else if (metamacCriteriaRestriction instanceof MetamacCriteriaPropertyRestriction) {
            addRestriction((MetamacCriteriaPropertyRestriction) metamacCriteriaRestriction, criteria);
        } else {
            throw new MetamacException(CommonServiceExceptionType.PARAMETER_INCORRECT, "CRITERIA");
        }
    }

    private void addRestriction(MetamacCriteriaDisjunctionRestriction disjunction, ConditionRoot criteria) throws MetamacException {
        criteria.lbrace();
        for (int i = 0; i < disjunction.getRestrictions().getRestriction().size(); i++) {
            MetamacCriteriaRestriction metamacCriteriaSubrestriction = disjunction.getRestrictions().getRestriction().get(i);
            addRestriction(metamacCriteriaSubrestriction, criteria);
            if (i < disjunction.getRestrictions().getRestriction().size() - 1) {
                criteria.or();
            }
        }
        criteria.rbrace();
    }

    private void addRestriction(MetamacCriteriaConjunctionRestriction conjunction, ConditionRoot criteria) throws MetamacException {
        criteria.lbrace();
        for (int i = 0; i < conjunction.getRestrictions().getRestriction().size(); i++) {
            MetamacCriteriaRestriction metamacCriteriaSubrestriction = conjunction.getRestrictions().getRestriction().get(i);
            addRestriction(metamacCriteriaSubrestriction, criteria);
            if (i < conjunction.getRestrictions().getRestriction().size() - 1) {
                criteria.and();
            }
        }
        criteria.rbrace();
    }

    private void addRestriction(MetamacCriteriaPropertyRestriction metamacCriteriaPropertyRestriction, ConditionRoot criteria) throws MetamacException {
        checkPropertyRestriction(metamacCriteriaPropertyRestriction);
        
        SculptorPropertyCriteria sculptorPropertyCriteria = callback.retrieveProperty(metamacCriteriaPropertyRestriction);
        CondditionProperty condditionProperty = criteria.withProperty(sculptorPropertyCriteria.getProperty());
        switch (metamacCriteriaPropertyRestriction.getOperation()) {
            case EQ:
                condditionProperty.eq(sculptorPropertyCriteria.getValue());
                break;
            case IEQ:
                condditionProperty.ignoreCaseEq(sculptorPropertyCriteria.getValue());
                break;
            case LIKE:
                condditionProperty.like(sculptorPropertyCriteria.getValue());
                break;
            case ILIKE:
                condditionProperty.ignoreCaseLike(sculptorPropertyCriteria.getValue());
                break;
            case IS_NULL:
                condditionProperty.isNull();
                break;
            case IS_NOT_NULL:
                condditionProperty.isNotNull();
                break;
            case NE:
                condditionProperty.eq(sculptorPropertyCriteria.getValue()).not();
                break;
            case LT:
                condditionProperty.lessThan(sculptorPropertyCriteria.getValue());
                break;
            case LE:
                condditionProperty.lessThanOrEqual(sculptorPropertyCriteria.getValue());
                break;
            case GT:
                condditionProperty.greaterThan(sculptorPropertyCriteria.getValue());
                break;
            case GE:
                condditionProperty.greaterThanOrEqual(sculptorPropertyCriteria.getValue());
                break;
            default:
                throw new MetamacException(CommonServiceExceptionType.PARAMETER_INCORRECT, "CRITERIA");
        }        
    }

    private void checkPropertyOrder(MetamacCriteriaOrder order) throws MetamacException {
        try {
            Enum.valueOf(propertyOrderEnumClass, order.getPropertyName());
        } catch (Throwable e) {
            throw new MetamacException(CommonServiceExceptionType.PARAMETER_INCORRECT, order.getPropertyName());
        }
    }

    private void checkPropertyRestriction(MetamacCriteriaPropertyRestriction propertyRestriction) throws MetamacException {
        try {
            Enum.valueOf(propertyRestrictionEnumClass, propertyRestriction.getPropertyName());
        } catch (Throwable e) {
            throw new MetamacException(CommonServiceExceptionType.PARAMETER_INCORRECT, propertyRestriction.getPropertyName());
        }
    }

    public static interface CriteriaCallback {

        public SculptorPropertyCriteria retrieveProperty(MetamacCriteriaPropertyRestriction propertyRestriction) throws MetamacException;
        public Property retrievePropertyOrder(MetamacCriteriaOrder order) throws MetamacException;
        public Property retrievePropertyOrderDefault() throws MetamacException;
    }
}
