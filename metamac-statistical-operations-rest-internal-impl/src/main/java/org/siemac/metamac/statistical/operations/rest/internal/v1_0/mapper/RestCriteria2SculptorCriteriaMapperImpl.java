package org.siemac.metamac.statistical.operations.rest.internal.v1_0.mapper;

import java.util.Date;

import javax.ws.rs.core.Response.Status;

import org.fornax.cartridges.sculptor.framework.domain.LeafProperty;
import org.fornax.cartridges.sculptor.framework.domain.Property;
import org.siemac.metamac.core.common.constants.CoreCommonConstants;
import org.siemac.metamac.core.common.util.CoreCommonUtil;
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
import org.siemac.metamac.statistical.operations.core.domain.Instance;
import org.siemac.metamac.statistical.operations.core.domain.InstanceProperties;
import org.siemac.metamac.statistical.operations.core.domain.Operation;
import org.siemac.metamac.statistical.operations.core.domain.OperationProperties;
import org.siemac.metamac.statistical.operations.core.enume.domain.ProcStatusEnum;
import org.siemac.metamac.statistical.operations.core.enume.domain.StatusEnum;
import org.siemac.metamac.statistical.operations.rest.internal.v1_0.domain.FamilyCriteriaPropertyOrder;
import org.siemac.metamac.statistical.operations.rest.internal.v1_0.domain.FamilyCriteriaPropertyRestriction;
import org.siemac.metamac.statistical.operations.rest.internal.v1_0.domain.InstanceCriteriaPropertyOrder;
import org.siemac.metamac.statistical.operations.rest.internal.v1_0.domain.InstanceCriteriaPropertyRestriction;
import org.siemac.metamac.statistical.operations.rest.internal.v1_0.domain.OperationCriteriaPropertyOrder;
import org.siemac.metamac.statistical.operations.rest.internal.v1_0.domain.OperationCriteriaPropertyRestriction;
import org.springframework.stereotype.Component;

@Component
public class RestCriteria2SculptorCriteriaMapperImpl implements RestCriteria2SculptorCriteriaMapper {

    private RestCriteria2SculptorCriteria<Operation> operationCriteriaMapper = null;
    private RestCriteria2SculptorCriteria<Instance>  instanceCriteriaMapper  = null;
    private RestCriteria2SculptorCriteria<Family>    familyCriteriaMapper    = null;

    public RestCriteria2SculptorCriteriaMapperImpl() {
        operationCriteriaMapper = new RestCriteria2SculptorCriteria<Operation>(Operation.class, OperationCriteriaPropertyOrder.class, OperationCriteriaPropertyRestriction.class,
                new OperationCriteriaCallback());
        familyCriteriaMapper = new RestCriteria2SculptorCriteria<Family>(Family.class, FamilyCriteriaPropertyOrder.class, FamilyCriteriaPropertyRestriction.class, new FamilyCriteriaCallback());
        instanceCriteriaMapper = new RestCriteria2SculptorCriteria<Instance>(Instance.class, InstanceCriteriaPropertyOrder.class, InstanceCriteriaPropertyRestriction.class,
                new InstanceCriteriaCallback());
    }

    @Override
    public RestCriteria2SculptorCriteria<Operation> getOperationCriteriaMapper() {
        return operationCriteriaMapper;
    }

    @Override
    public RestCriteria2SculptorCriteria<Instance> getInstanceCriteriaMapper() {
        return instanceCriteriaMapper;
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
                case ID:
                    return new SculptorPropertyCriteria(OperationProperties.code(), propertyRestriction.getValue());
                case URN:
                    return new SculptorPropertyCriteria(OperationProperties.urn(), propertyRestriction.getValue());
                case TITLE:
                    return new SculptorPropertyCriteria(OperationProperties.title().texts().label(), propertyRestriction.getValue());
                case SUBJECT_AREA_URN:
                    return new SculptorPropertyCriteria(OperationProperties.subjectArea().urn(), propertyRestriction.getValue());
                case SECONDARY_SUBJECT_AREA_URN:
                    return new SculptorPropertyCriteria(OperationProperties.secondarySubjectAreas().urn(), propertyRestriction.getValue());
                case DESCRIPTION:
                    return new SculptorPropertyCriteria(OperationProperties.description().texts().label(), propertyRestriction.getValue());
                case SURVEY_TYPE_ID:
                    return new SculptorPropertyCriteria(OperationProperties.surveyType().identifier(), propertyRestriction.getValue());
                case OFFICIALITY_TYPE_ID:
                    return new SculptorPropertyCriteria(OperationProperties.officialityType().identifier(), propertyRestriction.getValue());
                case IS_INDICATORS_SYSTEM:
                    return new SculptorPropertyCriteria(OperationProperties.indicatorSystem(), propertyRestrictionValueToBoolean(propertyRestriction.getValue()));
                case PRODUCER_URN:
                    return new SculptorPropertyCriteria(OperationProperties.producer().urn(), propertyRestriction.getValue());
                case INTERNAL_INVENTORY_DATE:
                    return new SculptorPropertyCriteria(new LeafProperty<Operation>(OperationProperties.internalInventoryDate().getName(), CoreCommonConstants.CRITERIA_DATETIME_COLUMN_DATETIME, true,
                            Operation.class), propertyRestrictionValueToDate(propertyRestriction.getValue()));
                case CURRENTLY_ACTIVE:
                    return new SculptorPropertyCriteria(OperationProperties.currentlyActive(), propertyRestrictionValueToBoolean(propertyRestriction.getValue()));
                case STATUS:
                    return new SculptorPropertyCriteria(OperationProperties.status(), propertyRestrictionValueToStatusEnum(propertyRestriction.getValue()));
                case PROC_STATUS:
                    return new SculptorPropertyCriteria(OperationProperties.procStatus(), propertyRestrictionValueToProcStatusEnum(propertyRestriction.getValue()));
                case PUBLISHER_URN:
                    return new SculptorPropertyCriteria(OperationProperties.publisher().urn(), propertyRestriction.getValue());
                case INVENTORY_DATE:
                    return new SculptorPropertyCriteria(new LeafProperty<Operation>(OperationProperties.inventoryDate().getName(), CoreCommonConstants.CRITERIA_DATETIME_COLUMN_DATETIME, true,
                            Operation.class), propertyRestrictionValueToDate(propertyRestriction.getValue()));
                default:
                    throw toRestExceptionParameterIncorrect(propertyNameCriteria.name());
            }
        }

        @SuppressWarnings("rawtypes")
        @Override
        public Property retrievePropertyOrder(MetamacRestOrder order) throws RestException {
            OperationCriteriaPropertyOrder propertyNameCriteria = OperationCriteriaPropertyOrder.fromValue(order.getPropertyName());
            switch (propertyNameCriteria) {
                case ID:
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
                case ID:
                    return new SculptorPropertyCriteria(FamilyProperties.code(), propertyRestriction.getValue());
                case URN:
                    return new SculptorPropertyCriteria(FamilyProperties.urn(), propertyRestriction.getValue());
                case TITLE:
                    return new SculptorPropertyCriteria(FamilyProperties.title().texts().label(), propertyRestriction.getValue());
                case ACRONYM:
                    return new SculptorPropertyCriteria(FamilyProperties.acronym().texts().label(), propertyRestriction.getValue());
                case DESCRIPTION:
                    return new SculptorPropertyCriteria(FamilyProperties.description().texts().label(), propertyRestriction.getValue());
                case INTERNAL_INVENTORY_DATE:
                    return new SculptorPropertyCriteria(new LeafProperty<Family>(FamilyProperties.internalInventoryDate().getName(), CoreCommonConstants.CRITERIA_DATETIME_COLUMN_DATETIME, true,
                            Family.class), propertyRestrictionValueToDate(propertyRestriction.getValue()));
                case PROC_STATUS:
                    return new SculptorPropertyCriteria(FamilyProperties.procStatus(), propertyRestrictionValueToProcStatusEnum(propertyRestriction.getValue()));
                case INVENTORY_DATE:
                    return new SculptorPropertyCriteria(
                            new LeafProperty<Family>(FamilyProperties.inventoryDate().getName(), CoreCommonConstants.CRITERIA_DATETIME_COLUMN_DATETIME, true, Family.class),
                            propertyRestrictionValueToDate(propertyRestriction.getValue()));
                default:
                    throw toRestExceptionParameterIncorrect(propertyNameCriteria.name());
            }
        }
        @SuppressWarnings("rawtypes")
        @Override
        public Property retrievePropertyOrder(MetamacRestOrder order) throws RestException {
            FamilyCriteriaPropertyOrder propertyNameCriteria = FamilyCriteriaPropertyOrder.fromValue(order.getPropertyName());
            switch (propertyNameCriteria) {
                case ID:
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

    private class InstanceCriteriaCallback implements CriteriaCallback {

        @Override
        public SculptorPropertyCriteria retrieveProperty(MetamacRestQueryPropertyRestriction propertyRestriction) throws RestException {
            InstanceCriteriaPropertyRestriction propertyNameCriteria = InstanceCriteriaPropertyRestriction.fromValue(propertyRestriction.getPropertyName());
            switch (propertyNameCriteria) {
                case ID:
                    return new SculptorPropertyCriteria(InstanceProperties.code(), propertyRestriction.getValue());
                case URN:
                    return new SculptorPropertyCriteria(InstanceProperties.urn(), propertyRestriction.getValue());
                case TITLE:
                    return new SculptorPropertyCriteria(InstanceProperties.title().texts().label(), propertyRestriction.getValue());
                case ACRONYM:
                    return new SculptorPropertyCriteria(InstanceProperties.acronym().texts().label(), propertyRestriction.getValue());
                case DATA_DESCRIPTION:
                    return new SculptorPropertyCriteria(InstanceProperties.dataDescription().texts().label(), propertyRestriction.getValue());
                case GEOGRAPHIC_GRANULARITY_URN:
                    return new SculptorPropertyCriteria(InstanceProperties.geographicGranularity().urn(), propertyRestriction.getValue());
                case TEMPORAL_GRANULARITY_URN:
                    return new SculptorPropertyCriteria(InstanceProperties.temporalGranularity().urn(), propertyRestriction.getValue());
                case INSTANCE_TYPE_ID:
                    return new SculptorPropertyCriteria(InstanceProperties.instanceType().identifier(), propertyRestriction.getValue());
                case INTERNAL_INVENTORY_DATE:
                    return new SculptorPropertyCriteria(new LeafProperty<Instance>(InstanceProperties.internalInventoryDate().getName(), CoreCommonConstants.CRITERIA_DATETIME_COLUMN_DATETIME, true,
                            Instance.class), propertyRestrictionValueToDate(propertyRestriction.getValue()));
                case PROC_STATUS:
                    return new SculptorPropertyCriteria(InstanceProperties.procStatus(), propertyRestrictionValueToProcStatusEnum(propertyRestriction.getValue()));
                case INVENTORY_DATE:
                    return new SculptorPropertyCriteria(new LeafProperty<Instance>(InstanceProperties.inventoryDate().getName(), CoreCommonConstants.CRITERIA_DATETIME_COLUMN_DATETIME, true,
                            Instance.class), propertyRestrictionValueToDate(propertyRestriction.getValue()));
                default:
                    throw toRestExceptionParameterIncorrect(propertyNameCriteria.name());
            }
        }

        @SuppressWarnings("rawtypes")
        @Override
        public Property retrievePropertyOrder(MetamacRestOrder order) throws RestException {
            InstanceCriteriaPropertyOrder propertyNameCriteria = InstanceCriteriaPropertyOrder.fromValue(order.getPropertyName());
            switch (propertyNameCriteria) {
                case ID:
                    return InstanceProperties.code();
                default:
                    throw toRestExceptionParameterIncorrect(propertyNameCriteria.name());
            }
        }

        @SuppressWarnings("rawtypes")
        @Override
        public Property retrievePropertyOrderDefault() throws RestException {
            return InstanceProperties.id();
        }
    }

    private RestException toRestExceptionParameterIncorrect(String parameter) {
        Error error = RestExceptionUtils.getError(RestCommonServiceExceptionType.PARAMETER_INCORRECT, parameter);
        return new RestException(error, Status.INTERNAL_SERVER_ERROR);
    }

    private ProcStatusEnum propertyRestrictionValueToProcStatusEnum(String value) {
        return value != null ? ProcStatusEnum.valueOf(value) : null;
    }

    private Boolean propertyRestrictionValueToBoolean(String value) {
        return value != null ? Boolean.valueOf(value) : null;
    }

    private Date propertyRestrictionValueToDate(String value) {
        return value != null ? CoreCommonUtil.transformISODateTimeLexicalRepresentationToDateTime(value).toDate() : null;
    }

    private StatusEnum propertyRestrictionValueToStatusEnum(String value) {
        return value != null ? StatusEnum.valueOf(value) : null;
    }
}
