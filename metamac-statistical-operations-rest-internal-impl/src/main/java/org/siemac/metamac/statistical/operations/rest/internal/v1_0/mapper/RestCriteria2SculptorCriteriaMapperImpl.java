package org.siemac.metamac.statistical.operations.rest.internal.v1_0.mapper;

import javax.ws.rs.core.Response.Status;

import org.fornax.cartridges.sculptor.framework.domain.Property;
import org.siemac.metamac.rest.common.query.domain.MetamacRestOrder;
import org.siemac.metamac.rest.common.query.domain.MetamacRestQueryPropertyRestriction;
import org.siemac.metamac.rest.common.query.domain.SculptorPropertyCriteria;
import org.siemac.metamac.rest.common.v1_0.domain.Error;
import org.siemac.metamac.rest.exception.RestCommonServiceExceptionType;
import org.siemac.metamac.rest.exception.RestException;
import org.siemac.metamac.rest.exception.utils.RestExceptionUtils;
import org.siemac.metamac.rest.search.criteria.mapper.RestCriteria2SculptorCriteria;
import org.siemac.metamac.rest.search.criteria.mapper.RestCriteria2SculptorCriteria.CriteriaCallback;
import org.siemac.metamac.statistical.operations.core.domain.Family;
import org.siemac.metamac.statistical.operations.core.domain.FamilyProperties;
import org.siemac.metamac.statistical.operations.core.domain.Operation;
import org.siemac.metamac.statistical.operations.core.domain.OperationProperties;
import org.siemac.metamac.statistical.operations.core.enume.domain.ProcStatusEnum;
import org.siemac.metamac.statistical.operations.rest.internal.v1_0.domain.FamilyCriteriaPropertyOrder;
import org.siemac.metamac.statistical.operations.rest.internal.v1_0.domain.FamilyCriteriaPropertyRestriction;
import org.siemac.metamac.statistical.operations.rest.internal.v1_0.domain.OperationCriteriaPropertyOrder;
import org.siemac.metamac.statistical.operations.rest.internal.v1_0.domain.OperationCriteriaPropertyRestriction;
import org.springframework.stereotype.Component;

@Component
public class RestCriteria2SculptorCriteriaMapperImpl implements RestCriteria2SculptorCriteriaMapper {

    private RestCriteria2SculptorCriteria<Operation> operationCriteriaMapper = null;
    private RestCriteria2SculptorCriteria<Family>    familyCriteriaMapper    = null;

    public RestCriteria2SculptorCriteriaMapperImpl() {
        operationCriteriaMapper = new RestCriteria2SculptorCriteria<Operation>(Operation.class, OperationCriteriaPropertyOrder.class, OperationCriteriaPropertyRestriction.class,
                new OperationCriteriaCallback());
        familyCriteriaMapper = new RestCriteria2SculptorCriteria<Family>(Family.class, FamilyCriteriaPropertyOrder.class, FamilyCriteriaPropertyRestriction.class, new FamilyCriteriaCallback());
    }

    @Override
    public RestCriteria2SculptorCriteria<Operation> getOperationCriteriaMapper() {
        return operationCriteriaMapper;
    }

    @Override
    public RestCriteria2SculptorCriteria<Family> getFamilyCriteriaMapper() {
        return familyCriteriaMapper;
    }

    private class OperationCriteriaCallback implements CriteriaCallback {

        @Override
        public SculptorPropertyCriteria retrieveProperty(MetamacRestQueryPropertyRestriction propertyRestriction) throws RestException {
            OperationCriteriaPropertyRestriction propertyNameCriteria = OperationCriteriaPropertyRestriction.fromValue(propertyRestriction.getPropertyName());
            switch (propertyNameCriteria) {
                case CODE:
                    return new SculptorPropertyCriteria(OperationProperties.code(), propertyRestriction.getValue());
                case PROC_STATUS:
                    ProcStatusEnum procStatus = propertyRestriction.getValue() != null ? ProcStatusEnum.valueOf(propertyRestriction.getValue()) : null;
                    return new SculptorPropertyCriteria(OperationProperties.procStatus(), procStatus);
                case IS_INDICATORS_SYSTEM:
                    Boolean isIndicatorsSystem = propertyRestriction.getValue() != null ? Boolean.valueOf(propertyRestriction.getValue()) : null;
                    return new SculptorPropertyCriteria(OperationProperties.indicatorSystem(), isIndicatorsSystem);
                default:
                    throw toRestExceptionParameterIncorrect(propertyNameCriteria.name());
            }
        }

        @SuppressWarnings("rawtypes")
        @Override
        public Property retrievePropertyOrder(MetamacRestOrder order) throws RestException {
            OperationCriteriaPropertyOrder propertyNameCriteria = OperationCriteriaPropertyOrder.fromValue(order.getPropertyName());
            switch (propertyNameCriteria) {
                case CODE:
                    return OperationProperties.code();
                default:
                    throw toRestExceptionParameterIncorrect(propertyNameCriteria.name());
            }
        }

        @SuppressWarnings("rawtypes")
        @Override
        public Property retrievePropertyOrderDefault() throws RestException {
            return OperationProperties.id();
        }
    }

    private class FamilyCriteriaCallback implements CriteriaCallback {

        @Override
        public SculptorPropertyCriteria retrieveProperty(MetamacRestQueryPropertyRestriction propertyRestriction) throws RestException {
            FamilyCriteriaPropertyRestriction propertyNameCriteria = FamilyCriteriaPropertyRestriction.fromValue(propertyRestriction.getPropertyName());
            switch (propertyNameCriteria) {
                case CODE:
                    return new SculptorPropertyCriteria(FamilyProperties.code(), propertyRestriction.getValue());
                case PROC_STATUS:
                    ProcStatusEnum value = propertyRestriction.getValue() != null ? ProcStatusEnum.valueOf(propertyRestriction.getValue()) : null;
                    return new SculptorPropertyCriteria(FamilyProperties.procStatus(), value);
                default:
                    throw toRestExceptionParameterIncorrect(propertyNameCriteria.name());
            }
        }

        @SuppressWarnings("rawtypes")
        @Override
        public Property retrievePropertyOrder(MetamacRestOrder order) throws RestException {
            FamilyCriteriaPropertyOrder propertyNameCriteria = FamilyCriteriaPropertyOrder.fromValue(order.getPropertyName());
            switch (propertyNameCriteria) {
                case CODE:
                    return FamilyProperties.code();
                default:
                    throw toRestExceptionParameterIncorrect(propertyNameCriteria.name());
            }
        }

        @SuppressWarnings("rawtypes")
        @Override
        public Property retrievePropertyOrderDefault() throws RestException {
            return FamilyProperties.id();
        }
    }

    private RestException toRestExceptionParameterIncorrect(String parameter) {
        Error error = RestExceptionUtils.getError(RestCommonServiceExceptionType.PARAMETER_INCORRECT, parameter);
        return new RestException(error, Status.INTERNAL_SERVER_ERROR);
    }
}
