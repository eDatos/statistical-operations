package org.siemac.metamac.statistical.operations.core.mapper.ws.external;

import org.fornax.cartridges.sculptor.framework.domain.Property;
import org.siemac.metamac.core.common.criteria.SculptorPropertyCriteria;
import org.siemac.metamac.core.common.exception.MetamacException;
import org.siemac.metamac.schema.common.v1_0.domain.MetamacCriteriaOrder;
import org.siemac.metamac.schema.common.v1_0.domain.MetamacCriteriaPropertyRestriction;
import org.siemac.metamac.statistical.operations.core.domain.Operation;
import org.siemac.metamac.statistical.operations.core.domain.OperationProperties;
import org.siemac.metamac.statistical.operations.core.error.ServiceExceptionType;
import org.siemac.metamac.statistical.operations.core.ws.criteria.mapper.MetamacCriteriaWebService2SculptorCriteria;
import org.siemac.metamac.statistical.operations.core.ws.criteria.mapper.MetamacCriteriaWebService2SculptorCriteria.CriteriaCallback;
import org.siemac.metamac.statistical.operations.external.ws.v1_0.domain.OperationCriteriaPropertyRestriction;
import org.springframework.stereotype.Component;

@Component
public class MetamacCriteriaWebServiceExternal2SculptorCriteriaMapperImpl implements MetamacCriteriaWebServiceExternal2SculptorCriteriaMapper {

    private MetamacCriteriaWebService2SculptorCriteria<Operation> operationCriteriaMapper = null;

    public MetamacCriteriaWebServiceExternal2SculptorCriteriaMapperImpl() throws MetamacException {
        operationCriteriaMapper = new MetamacCriteriaWebService2SculptorCriteria<Operation>(Operation.class, null, OperationCriteriaPropertyRestriction.class, new OperationCriteriaCallback());
    }

    @Override
    public MetamacCriteriaWebService2SculptorCriteria<Operation> getOperationCriteriaMapper() {
        return operationCriteriaMapper;
    }

    private class OperationCriteriaCallback implements CriteriaCallback {

        @Override
        public SculptorPropertyCriteria retrieveProperty(MetamacCriteriaPropertyRestriction propertyRestriction) throws MetamacException {
            OperationCriteriaPropertyRestriction propertyNameCriteria = OperationCriteriaPropertyRestriction.fromValue(propertyRestriction.getPropertyName());
            switch (propertyNameCriteria) {
                case CODE:
                    return new SculptorPropertyCriteria(OperationProperties.code(), propertyRestriction.getStringValue());
                case IS_INDICATORS_SYSTEM:
                    return new SculptorPropertyCriteria(OperationProperties.indicatorSystem(), propertyRestriction.getBooleanValue());
                default:
                    throw new MetamacException(ServiceExceptionType.PARAMETER_INCORRECT, propertyRestriction.getPropertyName());
            }
        }

        @Override
        public Property<Operation> retrievePropertyOrder(MetamacCriteriaOrder order) throws MetamacException {
            return null; // put default order
        }

        @Override
        public Property<Operation> retrievePropertyOrderDefault() throws MetamacException {
            return OperationProperties.id();
        }
    }
}
